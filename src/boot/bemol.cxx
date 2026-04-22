// -- -*- C++ -*-
// Copyright (C) 2026, Gabriel Dos Reis.
// All rights reserved.
// Written by Gabriel Dos Reis.
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
//     - Neither the name of OpenAxiom, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
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

// --% Author: Gabriel Dos Reis
// --% Description:
// --%   Test driver for the Boot language parser.  Reads each
// --%   input .boot file, tokenizes every fragment, runs the
// --%   recursive-descent parser, and reports statistics or errors.

#include <iostream>
#include <fstream>
#include <string>
#include <string_view>
#include <vector>

#include <open-axiom/Charset>
#include <open-axiom/InputFragment>
#include <open-axiom/token>
#include <open-axiom/SyntaxTree>
#include <open-axiom/SyntaxPrint>
#include <open-axiom/Parser>
#include <open-axiom/LispValue>
#include <open-axiom/BootAst>
#include <open-axiom/BootTranslator>
#include <open-axiom/diagnostics>
#include <open-axiom/FileMapping>

namespace {
    using namespace OpenAxiom;

    // -- Processing options for a Boot source file.
    //    Each flag selects an output phase.
    enum class ProcessingMode : unsigned {
        None      = 0,
        Verbose   = 1 << 0,   // Print AST node kinds.
        Dump      = 1 << 1,   // Print CST as S-expressions.
        Lower     = 1 << 2,   // Lower CST to old-style AST.
        Translate = 1 << 3,   // Full Boot-to-Lisp translation.
    };

    constexpr ProcessingMode operator|(ProcessingMode a, ProcessingMode b)
    {
        return static_cast<ProcessingMode>(
            static_cast<unsigned>(a) | static_cast<unsigned>(b));
    }
    constexpr ProcessingMode operator&(ProcessingMode a, ProcessingMode b)
    {
        return static_cast<ProcessingMode>(
            static_cast<unsigned>(a) & static_cast<unsigned>(b));
    }
    constexpr bool has(ProcessingMode flags, ProcessingMode bit)
    {
        return (flags & bit) != ProcessingMode::None;
    }

    // -- Parsed command-line options.
    struct CommandLine {
        ProcessingMode mode = ProcessingMode::None;
        Language dialect = Language::Boot;
        const char* output_path = nullptr;      // -o file: exact output file (single input only)
        const char* output_dir = nullptr;        // --output-dir: directory for derived names
        bool use_stdout = false;                 // -o -: write to stdout
        std::vector<const char*> input_files;
    };

    // -- Summary statistics for a single input file.
    struct FileStats {
        const char* path;           // input file path
        int fragments;              // number of fragments processed
        int tokens;                 // total tokens across all fragments
        int top_level_forms;        // successful top-level parses
        int errors;                 // number of parse errors
        int unconsumed;             // tokens left after parsing

        FileStats& operator+=(const FileStats& rhs)
        {
            fragments += rhs.fragments;
            tokens += rhs.tokens;
            top_level_forms += rhs.top_level_forms;
            errors += rhs.errors;
            unconsumed += rhs.unconsumed;
            return *this;
        }
    };

    // -- Format a Kind as a readable string.
    const char* kind_name(Syntax::Kind k)
    {
        switch (k)
        {
        case Syntax::Kind::Literal:
            return "Literal";
        case Syntax::Kind::Name:
            return "Name";
        case Syntax::Kind::QualifiedName:
            return "QualifiedName";
        case Syntax::Kind::Module:
            return "Module";
        case Syntax::Kind::Namespace:
            return "Namespace";
        case Syntax::Kind::Import:
            return "Import";
        case Syntax::Kind::ImportSignature:
            return "ImportSignature";
        case Syntax::Kind::ConstantDef:
            return "ConstantDef";
        case Syntax::Kind::FunctionDef:
            return "FunctionDef";
        case Syntax::Kind::MacroDef:
            return "MacroDef";
        case Syntax::Kind::TypeAlias:
            return "TypeAlias";
        case Syntax::Kind::Signature:
            return "Signature";
        case Syntax::Kind::Apply:
            return "Apply";
        case Syntax::Kind::InfixExpr:
            return "InfixExpr";
        case Syntax::Kind::PrefixExpr:
            return "PrefixExpr";
        case Syntax::Kind::SuffixDot:
            return "SuffixDot";
        case Syntax::Kind::Restrict:
            return "Restrict";
        case Syntax::Kind::Coerce:
            return "Coerce";
        case Syntax::Kind::Quote:
            return "Quote";
        case Syntax::Kind::Bracket:
            return "Bracket";
        case Syntax::Kind::Tuple:
            return "Tuple";
        case Syntax::Kind::Sequence:
            return "Sequence";
        case Syntax::Kind::Pile:
            return "Pile";
        case Syntax::Kind::Mapping:
            return "Mapping";
        case Syntax::Kind::Forall:
            return "Forall";
        case Syntax::Kind::IfExpr:
            return "IfExpr";
        case Syntax::Kind::Return:
            return "Return";
        case Syntax::Kind::Leave:
            return "Leave";
        case Syntax::Kind::Implies:
            return "Implies";
        case Syntax::Kind::While:
            return "While";
        case Syntax::Kind::Until:
            return "Until";
        case Syntax::Kind::ForIn:
            return "ForIn";
        case Syntax::Kind::SuchThat:
            return "SuchThat";
        case Syntax::Kind::Iterators:
            return "Iterators";
        case Syntax::Kind::Cross:
            return "Cross";
        case Syntax::Kind::Repeat:
            return "Repeat";
        case Syntax::Kind::Is:
            return "Is";
        case Syntax::Kind::Isnt:
            return "Isnt";
        case Syntax::Kind::EqualPattern:
            return "EqualPattern";
        case Syntax::Kind::ColonAppend:
            return "ColonAppend";
        case Syntax::Kind::Assignment:
            return "Assignment";
        case Syntax::Kind::Lambda:
            return "Lambda";
        case Syntax::Kind::DefaultValue:
            return "DefaultValue";
        case Syntax::Kind::KeyArg:
            return "KeyArg";
        case Syntax::Kind::BoundedSegment:
            return "BoundedSegment";
        case Syntax::Kind::UnboundedSegment:
            return "UnboundedSegment";
        case Syntax::Kind::Reduce:
            return "Reduce";
        case Syntax::Kind::Structure:
            return "Structure";
        case Syntax::Kind::Record:
            return "Record";
        case Syntax::Kind::AccessorDef:
            return "AccessorDef";
        case Syntax::Kind::Case:
            return "Case";
        case Syntax::Kind::Throw:
            return "Throw";
        case Syntax::Kind::Catch:
            return "Catch";
        case Syntax::Kind::Finally:
            return "Finally";
        case Syntax::Kind::Try:
            return "Try";
        case Syntax::Kind::Where:
            return "Where";
        case Syntax::Kind::Dynamic:
            return "Dynamic";
        case Syntax::Kind::With:
            return "With";
        case Syntax::Kind::Add:
            return "Add";
        case Syntax::Kind::Capsule:
            return "Capsule";
        case Syntax::Kind::Pretend:
            return "Pretend";
        case Syntax::Kind::Has:
            return "Has";
        case Syntax::Kind::Collect:
            return "Collect";
        case Syntax::Kind::Join:
            return "Join";
        case Syntax::Kind::FreeDecl:
            return "FreeDecl";
        case Syntax::Kind::ExportDecl:
            return "ExportDecl";
        case Syntax::Kind::LispExpr:
            return "LispExpr";
        case Syntax::Kind::Unparsed:
            return "Unparsed";
        default:
            return "???";
        }
    }

    // -- Check whether a filename has the given suffix (case-insensitive).
    bool has_suffix(std::string_view path, std::string_view suffix)
    {
        if (path.size() < suffix.size())
            return false;
        auto tail = path.substr(path.size() - suffix.size());
        for (std::size_t i = 0; i < suffix.size(); ++i)
            if (ascii_lower(char8_t(tail[i])) != ascii_lower(char8_t(suffix[i])))
                return false;
        return true;
    }

    // -- Determine the language dialect for a file.
    //    If an explicit dialect was requested (via --spad), use it.
    //    Otherwise, infer from the file extension.
    Language dialect_for(const char* path, Language explicit_dialect)
    {
        if (explicit_dialect != Language::Boot)
            return explicit_dialect;
        if (has_suffix(path, ".spad"))
            return Language::Spad;
        return Language::Boot;
    }

    // -- Parse command-line arguments into a CommandLine structure.
    //    Returns false if a fatal error occurred (e.g. missing argument).
    bool parse_command_line(int argc, char* argv[], CommandLine& cl)
    {
        for (int i = 1; i < argc; ++i)
        {
            std::string_view arg = argv[i];
            if (arg == "-v" or arg == "--verbose")
                cl.mode = cl.mode | ProcessingMode::Verbose;
            else if (arg == "--dump")
                cl.mode = cl.mode | ProcessingMode::Dump;
            else if (arg == "--lower")
                cl.mode = cl.mode | ProcessingMode::Lower;
            else if (arg == "--translate")
                cl.mode = cl.mode | ProcessingMode::Lower
                                  | ProcessingMode::Translate;
            else if (arg == "--spad")
                cl.dialect = Language::Spad;
            else if (arg == "-o" or arg == "--output")
            {
                if (i + 1 < argc)
                {
                    ++i;
                    if (std::string_view{argv[i]} == "-")
                        cl.use_stdout = true;
                    else
                        cl.output_path = argv[i];
                }
                else
                {
                    std::cerr << "error: " << arg
                              << " requires an argument\n";
                    return false;
                }
            }
            else if (arg == "--output-dir")
            {
                if (i + 1 < argc)
                    cl.output_dir = argv[++i];
                else
                {
                    std::cerr << "error: " << arg
                              << " requires an argument\n";
                    return false;
                }
            }
            else
                cl.input_files.push_back(argv[i]);
        }
        // -- Validate: -o (file) is only valid with a single input file.
        if (cl.output_path != nullptr and cl.input_files.size() > 1)
        {
            std::cerr << "error: -o/--output cannot be used with "
                         "multiple input files\n";
            return false;
        }
        // -- Validate: -o and --output-dir are mutually exclusive.
        if ((cl.output_path != nullptr or cl.use_stdout)
            and cl.output_dir != nullptr)
        {
            std::cerr << "error: -o/--output and --output-dir "
                         "are mutually exclusive\n";
            return false;
        }
        return true;
    }

    // -- Derive an output file path from an input file path.
    //    Replaces the extension with ".out".
    //    If output_dir is given, places the file there; otherwise
    //    the output is alongside the input.
    // -- Choose a file extension based on the processing mode and dialect.
    const char* output_extension(ProcessingMode mode, Language dialect)
    {
        if (dialect == Language::Boot
            and has(mode, ProcessingMode::Translate))
            return ".clisp";
        return ".out";
    }

    std::string derive_output_path(const char* input_path,
                                   const char* output_dir,
                                   ProcessingMode mode,
                                   Language dialect)
    {
        std::string_view in = input_path;
        // -- Find the basename (after last separator).
        auto sep = in.find_last_of("/\\");
        auto base = (sep != std::string_view::npos)
            ? in.substr(sep + 1) : in;
        // -- Strip the extension.
        auto dot = base.find_last_of('.');
        auto stem = (dot != std::string_view::npos)
            ? base.substr(0, dot) : base;

        std::string result;
        if (output_dir != nullptr)
        {
            result = output_dir;
            if (not result.empty() and result.back() != '/'
                and result.back() != '\\')
                result += '/';
            result += stem;
        }
        else
        {
            // -- Same directory as input.
            auto dir = (sep != std::string_view::npos)
                ? in.substr(0, sep + 1) : std::string_view{};
            result = dir;
            result += stem;
        }
        result += output_extension(mode, dialect);
        return result;
    }

    // -- Process a single source file.
    //    Returns the file statistics (fragments, tokens, forms, errors).
    //    If os is null, output phases are skipped (stats-only mode).
    //    When dialect is Language::Spad, only tokenization is performed.
    FileStats process_file(const char* path, std::ostream* os,
                           ProcessingMode mode,
                           Language dialect = Language::Boot)
    {
        FileStats stats { path, 0, 0, 0, 0, 0 };
        Memory::FileMapping file { path };
        if (file.empty())
        {
            std::cerr << "error: cannot open " << path << '\n';
            stats.errors = 1;
            return stats;
        }
        // -- Read source with reader directive evaluation.
        // *FEATURES* is nil -- no host Lisp features.
        Utf8SourceView source { file.begin(), file.size() };
        auto prose = read_source(source.view(), Lisp::nil);

        // -- Spad mode: tokenize and parse.
        if (dialect == Language::Spad)
        {
            Syntax::SyntaxForest forest;
            for (auto& frag : prose)
            {
                ++stats.fragments;
                try
                {
                    auto toks = pile(words(frag, Language::Spad));
                    stats.tokens += static_cast<int>(toks.size());
                    std::size_t consumed = 0;
                    auto forms = Boot::parse_spad(toks, forest, &consumed);
                    stats.top_level_forms += static_cast<int>(forms.size());
                    // -- Count significant (non-whitespace/comment/formatting)
                    // tokens left unconsumed after parsing.
                    for (std::size_t i = consumed; i < toks.size(); ++i)
                    {
                        switch (toks[i].category)
                        {
                        case TokenCategory::Whitespace:
                        case TokenCategory::Comment:
                        case TokenCategory::Formatting:
                            break;
                        default:
                            ++stats.unconsumed;
                            break;
                        }
                    }

                    if (os != nullptr and has(mode, ProcessingMode::Verbose))
                    {
                        for (auto node : forms)
                        {
                            auto k = Syntax::kind(node);
                            auto idx = Syntax::index(node);
                            *os << "  " << kind_name(k)
                                << "[" << idx << "]\n";
                        }
                    }
                    if (os != nullptr and has(mode, ProcessingMode::Dump))
                    {
                        for (auto node : forms)
                        {
                            Syntax::print_sexpr(*os, node,
                                                forest, toks, frag);
                            *os << '\n';
                        }
                    }
                }
                catch (const EndOfStringUnseen& e)
                {
                    ++stats.errors;
                    std::cerr << path << ": lexer error: "
                              << "unterminated string at line " << e.line
                              << ", column " << e.column << '\n';
                }
                catch (const MissingExponent& e)
                {
                    ++stats.errors;
                    std::cerr << path << ": lexer error: "
                              << "missing exponent at line " << e.line
                              << ", column " << e.column << '\n';
                }
                catch (const Diagnostics::BasicError& e)
                {
                    ++stats.errors;
                    std::cerr << path << ": parse error: "
                              << e.message() << '\n';
                }
            }
            return stats;
        }

        // -- Boot mode: full processing.
        Syntax::SyntaxForest forest;
        Lisp::Arena arena;
        Boot::TranslationUnit tu(arena);

        // -- Check if this file enables clamming via )eval directive.
        for (auto& frag : prose)
        {
            for (auto& line : frag)
            {
                if (line.find(u8")eval") != std::u8string::npos
                    and line.find(u8"$bfClamming") != std::u8string::npos
                    and line.find(u8"true") != std::u8string::npos)
                {
                    tu.clamming = true;
                    break;
                }
            }
            if (tu.clamming)
                break;
        }

        // -- Collect all top-level AST nodes for translation.
        std::vector<Lisp::Value> all_ast_nodes;

        for (auto& frag : prose)
        {
            ++stats.fragments;
            try
            {
                auto toks = pile(words(frag, Language::Boot));
                stats.tokens += static_cast<int>(toks.size());
                auto forms = Boot::parse_boot(toks, forest);
                stats.top_level_forms += static_cast<int>(forms.size());

                if (os != nullptr and has(mode, ProcessingMode::Verbose))
                {
                    for (auto node : forms)
                    {
                        auto k = Syntax::kind(node);
                        auto idx = Syntax::index(node);
                        *os << "  " << kind_name(k)
                            << "[" << idx << "]\n";
                    }
                }
                if (os != nullptr and has(mode, ProcessingMode::Dump))
                {
                    for (auto node : forms)
                    {
                        Syntax::print_sexpr(*os, node,
                                            forest, toks, frag);
                        *os << '\n';
                    }
                }
                if (has(mode, ProcessingMode::Lower))
                {
                    Boot::LoweringContext ctx{ forest, toks, frag, arena };
                    for (auto node : forms)
                    {
                        auto val = Boot::lower(node, ctx);
                        if (has(mode, ProcessingMode::Translate))
                            all_ast_nodes.push_back(val);
                        else if (os != nullptr)
                        {
                            Lisp::print(*os, val);
                            *os << '\n';
                        }
                    }
                }
            }
            catch (const Diagnostics::BasicError& e)
            {
                ++stats.errors;
                std::cerr << path << ": parse error: "
                          << e.message() << '\n';
            }
            catch (const EndOfStringUnseen& e)
            {
                ++stats.errors;
                std::cerr << path << ": lexer error: "
                          << "unterminated string at line " << e.line
                          << ", column " << e.column << '\n';
            }
            catch (const MissingExponent& e)
            {
                ++stats.errors;
                std::cerr << path << ": lexer error: "
                          << "missing exponent at line " << e.line
                          << ", column " << e.column << '\n';
            }
        }

        // -- Output full translation if requested.
        if (os != nullptr and has(mode, ProcessingMode::Translate)
            and not all_ast_nodes.empty())
        {
            auto translated = Boot::translate_file(tu, all_ast_nodes);
            for (auto form : translated)
            {
                Lisp::print(*os, form);
                *os << "\n\n";
            }
        }

        return stats;
    }
}

int main(int argc, char* argv[])
{
    using namespace OpenAxiom;

    CommandLine cl;
    if (not parse_command_line(argc, argv, cl))
        return 1;

    bool has_output = cl.output_path != nullptr
                   or cl.output_dir != nullptr
                   or cl.use_stdout
                   or cl.input_files.size() == 1;
    FileStats totals { "total", 0, 0, 0, 0, 0 };
    int file_count = 0;

    for (auto input : cl.input_files)
    {
        auto file_dialect = dialect_for(input, cl.dialect);

        // -- Determine the output stream for this file.
        std::ofstream file_out;
        std::ostream* os = nullptr;
        if (cl.use_stdout)
        {
            os = &std::cout;
        }
        else if (cl.output_path != nullptr)
        {
            file_out.open(cl.output_path, std::ios_base::binary);
            if (not file_out)
            {
                std::cerr << "error: cannot open output file "
                          << cl.output_path << '\n';
                return 1;
            }
            os = &file_out;
        }
        else if (cl.output_dir != nullptr)
        {
            auto path = derive_output_path(input, cl.output_dir,
                                            cl.mode, file_dialect);
            file_out.open(path, std::ios_base::binary);
            if (not file_out)
            {
                std::cerr << "error: cannot open output file "
                          << path << '\n';
                return 1;
            }
            os = &file_out;
        }
        else if (cl.input_files.size() == 1)
        {
            auto path = derive_output_path(input, nullptr,
                                            cl.mode, file_dialect);
            file_out.open(path, std::ios_base::binary);
            if (not file_out)
            {
                std::cerr << "error: cannot open output file "
                          << path << '\n';
                return 1;
            }
            os = &file_out;
        }

        auto stats = process_file(input, os, cl.mode, file_dialect);
        totals += stats;
        ++file_count;

        if (stats.errors > 0)
            std::cerr << stats.path << ": "
                      << stats.errors << " error(s)\n";
        if (stats.unconsumed > 0)
            std::cerr << stats.path << ": "
                      << stats.unconsumed << " unconsumed token(s)\n";
    }

    // -- Summary goes to stderr when output is directed elsewhere,
    // otherwise to stdout.
    auto& summary = has_output ? std::cerr : std::cout;
    summary << "\n=== Parser Summary ===\n"
            << "  Files:      " << file_count << '\n'
            << "  Fragments:  " << totals.fragments << '\n'
            << "  Tokens:     " << totals.tokens << '\n'
            << "  Top-levels: " << totals.top_level_forms << '\n'
            << "  Unconsumed: " << totals.unconsumed << '\n'
            << "  Errors:     " << totals.errors << '\n';

    return totals.errors > 0 ? 1 : 0;
}
