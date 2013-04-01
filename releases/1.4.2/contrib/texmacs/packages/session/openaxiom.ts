<TeXmacs|WinTeXmacs-1.0.4.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|openaxiom|1.0>

    <\src-purpose>
      Markup for OpenAxiom sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|openaxiom-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<resize|<arg|prompt>|||1.75fn||>|<arg|body>>>>>

  <assign|openaxiom-output|<macro|body|<style-with|src-compact|none|<surround|<vspace*|1fn>|<vspace|1fn>|<with|par-left|<plus|<value|par-left>|1.75fn>|<generic-output*|<arg|body>>>>>>>

  \;

  <assign|leqno|<macro|<htab|5mm>>>

  <assign|openaxiomtype|<macro|type|<vspace*|0.5fn><next-line><hflush><with|color|brown|Type:
  <arg|type>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>