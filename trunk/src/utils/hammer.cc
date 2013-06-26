// Copyright (C) 2013, Gabriel Dos Reis.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of The Numerical Algorithms Group Ltd. nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// --% Author: Gabriel Dos Reis.
// --% Description:
// --%   This program implements basic functionalities for untangling
// --%   algebra source code from the pamphlets.  The syntax is that
// --%   of `noweb'.  A chunk definition starts with a pattern
// --%   <<name>>= on a line by itself, and ends with `@' by itself
// --%   on a line.  A chunk can refer to another chunk through
// --%   a pattern of the form `<<name>>'.

#include <string.h>
#include <stdlib.h>
#include <utility>
#include <string>
#include <iostream>
#include <fstream>
#include <iterator>
#include <list>
#include <vector>
#include <map>
#include <open-axiom/storage>
#include <open-axiom/FileMapping>

namespace OpenAxiom {
   namespace Hammer {
      // -------------
      // -- Element --
      // -------------
      // Base class of document elements.
      struct Element {
         virtual ~Element() { }
      };

      // ---------------
      // -- BasicText --
      // ---------------
      // Plain text, with no reference to any chunk.  
      struct BasicText : Element {
         BasicText(const Byte* f, const Byte* l) : span(f, l) { }
         // Pointer to the start of this basic text element
         const Byte* begin() const { return span.first; }
         // One-past-the-end of the this basic text element.
         const Byte* end() const { return span.second; }
      private:
         std::pair<const Byte*, const Byte*> span;
      };

      // ---------------
      // -- Reference --
      // ---------------
      // Reference to a a chunk by name.
      struct Reference : Element {
         explicit Reference(const std::string& s) : label(s) { }
         // Naame of the chunk referenced.
         const std::string& name() const { return label; }
      private:
         const std::string label;
      };

      // -------------------
      // -- CompositeText --
      // -------------------
      // Sequence of basic elements and reference to chunks.
      struct CompositeText: private std::vector<const Element*> {
         typedef std::vector<const Element*> base;
         using base::iterator;
         using base::begin;
         using base::end;
         using base::size;
         using base::operator[];

         // Augment this chunk with a basic text in the open interval
         // [f,l).
         CompositeText& add_text(const Byte* f, const Byte* l) {
            texts.push_back(BasicText(f, l));
            push_back(&texts.back());
            return *this;
         }

         // Augment this chunk with a reference to another chunk
         // named `n'.  Note that we don't attempt to check for
         // possible circularities.
         CompositeText reference_chunk(const Byte* f, const Byte* l) {
            refs.push_back(Reference(std::string(f, l)));
            push_back(&refs.back());
            return *this;
         }

      private:
         std::list<BasicText> texts;
         std::list<Reference> refs;
      };

      // --------------
      // -- Document --
      // --------------
      // A whole document; a sequence of chunks.
      struct Document : std::list<CompositeText> {
         Document(const Memory::FileMapping& file)
               : active_chunk(&prose), text_start(file.begin()) {
            parse(file);
         }

         // Return a pointer to a document chunk name `n'.
         // Otherwise, return null.
         CompositeText* lookup_chunk(const std::string& n) const {
            ChunkTable::const_iterator i = defs.find(n);
            return i == defs.end() ? 0 : i->second;
         }

      private:
         typedef std::map<std::string, CompositeText*> ChunkTable;
         CompositeText prose;         // the prose around the chunks.
         ChunkTable defs;             // chunk definition table.
         CompositeText* active_chunk; // chunk under construction.
         const Byte* text_start;      // begining of current basic text.

         // Append basic text in the range `[text_start,last)'
         // to the current chunk.
         void finish_chunk(const Byte* last) {
            if (text_start != last)
               active_chunk->add_text(text_start, last);
            active_chunk = &prose;
            text_start = last;
         }

         // Start a new chunk or extend an existing chunk.
         void begin_chunk(const std::string& name, const Byte* start) {
            if (CompositeText* chunk = lookup_chunk(name))
               active_chunk = chunk;
            else {
               push_back(CompositeText());
               defs[name] = active_chunk = &back();
            }
            text_start = start;
         }

         // Parse a file mapping into this document.
         void parse(const Memory::FileMapping&);
      };

      // Return true if the character `c' introduces a newline.
      static inline bool
      looking_at_newline(char c) {
         return c == '\n' or c == '\r';
      }

      // Attempt to advance the cursor past newline marker.
      // Return true on sucess.
      static bool
      saw_newline(const Byte*& cur, const Byte* end) {
         if (*cur == '\n') {
            ++cur;
            return true;
         }
         else if (*cur == '\r') {
            if (++cur < end and *cur == '\n')
               ++cur;
            return true;
         }
         return false;
      }

      // Move `cur' to end of line or `end', whichever comes first.
      // Return true if the area swept consisted only of blank characters.
      static inline bool
      trailing_blank(const Byte*& cur, const Byte* end) {
         bool result = true;
         for (; cur < end and not saw_newline(cur, end); ++cur)
            result = isspace(*cur);
         return result;
      }

      // Attempt to advance `cur' past the double left angle brackets
      // starting a chunk name.  Returm true on success.
      static bool
      chunk_name_began(const Byte*& cur, const Byte* end) {
         if (cur[0] == '<' and cur + 1 < end and cur[1] == '<') {
            cur += 2;
            return true;
         }
         return false;
      }

      // Attempt to move `cur' past the double right angle brackets
      // terminating a chunk name.  Returm true on success.
      static bool
      chunk_name_ended(const Byte*& cur, const Byte* end) {
         if (cur[0] == '>' and cur + 1 < end and cur[1] == '>') {
            cur += 2;
            return true;
         }
         return false;
      }

      // We've just seen the start of a chunk reference; skip
      // characters till we seen of the chunk's name.
      static void
      skip_to_end_of_chunk_name(const Byte*& cur, const Byte* end) {
         while (cur < end) {
            if (looking_at_newline(*cur)
                or (cur + 1 < end and cur[0] == '>' and cur[1] == '>'))
               return;
            ++cur;
         }
      }

      // Move the cursor until end of line.
      static void
      skip_to_end_of_line(const Byte*& cur, const Byte* end) {
         while (cur < end) {
            if (saw_newline(cur, end))
               break;
            ++cur;
         }
      }
      
      void
      Document::parse(const Memory::FileMapping& file) {
         auto cur = text_start;
         auto last = file.end();
         // Process one line at a time.
         while (cur < last) {
            // 1. `@' ends previous chunk
            if (*cur == '@') {
               auto p = cur;
               if (trailing_blank(++cur, last))
                  finish_chunk(p);
            }
            // 2. `<<' introduces a chunk reference or a chunk definition.
            else if (chunk_name_began(cur, last)) {
               auto label_start = cur;
               skip_to_end_of_chunk_name(cur, last);
               if (chunk_name_ended(cur, last)) {
                  auto label_end = cur - 2;
                  if (cur < last and *cur == '=') {
                     if (trailing_blank(++cur, last)) {
                        // chunk definition or extension
                        finish_chunk(label_start - 2);
                        begin_chunk(std::string(label_start, label_end), cur);
                     }
                  }
                  else if (trailing_blank(cur, last)) {
                     // This is just a reference to a chunk.
                     active_chunk->add_text(text_start, label_start - 2);
                     active_chunk->reference_chunk(label_start, label_end);
                     text_start = cur;
                  }
                  else
                     skip_to_end_of_line(cur, last);
               }
            }
            else
               skip_to_end_of_line(cur, last);
         }
         finish_chunk(cur);
      }

      // Capture  chunk resolution in a document.
      struct resolve_chunk {
         resolve_chunk(const std::string& s, const Document& f)
               : name(s), doc(f) { }
         const std::string name; // name of the chunk
         const Document& doc;    // document containing the chunk.
      };

      // Print the resolution of a chunk name onto an output stream.
      std::ostream&
      operator<<(std::ostream& os, const resolve_chunk& rc) {
         // FIXME: no attempt at detecting circularities.
         const CompositeText* doc = rc.doc.lookup_chunk(rc.name);
         if (doc == 0) {
            std::cerr << "chunk " << rc.name << " is undefined" << std::endl;
            exit(1);
         }
         for (std::size_t i = 0; i < doc->size(); ++i) {
            const Element* elt = (*doc)[i];
            if (const BasicText* t = dynamic_cast<const BasicText*>(elt))
               std::copy(t->begin(), t->end(),
                         std::ostream_iterator<char>(os));
            else if (const Reference* r = dynamic_cast<const Reference*>(elt))
               os << resolve_chunk(r->name(), rc.doc);
            else {
               std::cerr << "unknown document element" << std::endl;
               exit(1);
            }
         }

         return os;
      }

      // Return true if the `arg' is the option named`opt'.
      static inline bool
      is_option(const char* arg, const char* opt) {
         return strcmp(arg, opt) == 0;
      }

      // `arg' is a argument on the command line.  If `arg'
      // does not match option name `opt', return null.  Otherwise,
      // return a pointer to the terminating NUL character if there
      // is no specified value for that option, or a pointer to the
      // start of the value.
      static const char*
      is_named_arg(const char* arg, const char* opt) {
         const int n = strlen(opt);
         int i = 0;
         // Get out if argion name does not match.
         // Note:  Ideally, we could use strncmp().  However, that
         // function is not available in C++98, so we cannot depend on it.
         for (; i < n ; ++i)
            if (arg[i] != opt[i])
               return 0;

         if (arg[i] == '\0')
            return arg + i;     // no value for the option.
         return arg + n + 1;    // being of the value.
      }
   }
}


int
main(int argc, char* argv[]) {
   using namespace OpenAxiom::Hammer;
   int error_count = 0;
   const char* chunk = 0;      // chunck to tangle
   const char* output_path = 0; // path to the output file
   const char* input_path = 0;  // path to the input file.
   // 1. Process command line arguments.
   for (int pos = 1; error_count == 0 and pos < argc; ++pos) {
      if (const char* val = is_named_arg(argv[pos], "--tangle")) {
         if (chunk != 0) {
            std::cerr << "cannot tangle more than one chunk";
            ++error_count;
         }
         else
            chunk = *val == 0 ? "*" : val;
      }
      else if (const char* val = is_named_arg(argv[pos], "--output")) {
         if (*val == 0) {
            std::cerr << "missing output file name" << std::endl;
            ++error_count;
         }
         else
            output_path = val;
      }
      else if (argv[pos][0] == '-' and argv[pos][1] == '-') {
         std::cerr << "unknown option " << argv[pos] << std::endl;
         ++error_count;
      }
      else if (input_path != 0) {
         std::cerr << "there must be exactly one input file" << std::endl;
         ++error_count;
      }
      else
         input_path = argv[pos];
   }

   // 2. Basic sanity check.
   if (input_path == 0) {
      std::cerr << "missing input file" << std::endl;
      return 1;
   }
   if (output_path == 0) {
      std::cerr << "missing output file" << std::endl;
      return 1;
   }
   if (chunk == 0) {
      std::cerr << "missing chunk name" << std::endl;
      return 1;
   }

   if (error_count != 0)
      return 1;

   // 3. Attempt to extract the chunk.
   try {
      OpenAxiom::Memory::FileMapping file(input_path);
      std::ofstream os(output_path);
      os << resolve_chunk(chunk, Document(file));
   }
   catch(const OpenAxiom::SystemError& e) {
      std::cerr << e.message() << std::endl;
      exit(1);
   }
   return 0;
}
