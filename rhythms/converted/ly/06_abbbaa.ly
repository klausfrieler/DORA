\version "2.13.2"

#(ly:set-option 'point-and-click #f)

\header {
  title = "N.N"
  composer = "N.N"
  tagline = ##f
}
global =
{
    \override Staff.TimeSignature #'style = #'()
    \time 3/4
    \clef "treble"
    \key c \major
    \override Rest #'direction = #'0
    \override MultiMeasureRest #'staff-position = #0
}
\score
{
<<
    \transpose c' c'

    
    \new Staff
    <<
    \transpose c' c'

    {
      \global
  		%\override Score.MetronomeMark #'transparent = ##t
  		%\override Score.MetronomeMark #'stencil = ##f
  		
  		\override HorizontalBracket #'direction = #UP
  		\override HorizontalBracket #'bracket-flare = #'(0 . 0)
  		
  		\override TextSpanner #'dash-fraction = #1.0
  		\override TextSpanner #'bound-details #'left #'text = \markup{ \concat{\draw-line #'(0 . -1.0) \draw-line #'(1.0 . 0) }}
  		\override TextSpanner #'bound-details #'right #'text = \markup{ \concat{ \draw-line #'(1.0 . 0) \draw-line #'(0 . -1.0) }}
          \set Score.markFormatter = #format-mark-box-numbers


      \tempo 4 = 120
      \set Score.currentBarNumber = #1
     
      | \time 3/4 c''4 c''8 c''8 c''8 c''8 
      | c''8 c''8 c''4 c''4\bar  ".."
    }
    >>
>>    
}
