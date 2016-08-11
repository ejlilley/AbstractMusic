\version "2.10.33"

#(set-global-staff-size 15)
#(set-default-paper-size "a4" 'landscape)

up = { \change Staff = "upper" }
down = { \change Staff = "lower" }

shiftNote = { \once \override NoteColumn.force-hshift = #0.7
	      \shiftOnn }
	      

global = {
  \mergeDifferentlyHeadedOn
  \mergeDifferentlyDottedOn
  \showStaffSwitch
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 8/4
  s1*14
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 4/4
  s1
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 8/4
  s1*20
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 4/4
  s1
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 8/4
  s1*6
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 4/4
  s1*35
  \bar "||"
  s1*29
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 3/4
  s2.*8
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 6/8
  s2.*4
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 4/4
  s1*17
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 2/4
  s2
  \bar "||"
  \time 12/4
  s1.*28
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 6/4
  s1.
  \time 9/4
  s1. s2. s1. s2.
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 6/4
  s1.
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 9/4
  s1. s2. s1. s2. s1. s2. s1. s2. s1. s2.
  \bar "||"
  s1.*19 s2.*19
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 6/4
  s1.
  \bar "||"
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 4/4
  s1*7
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 8/4
  s1*4
  \bar "|."
}

upper = \relative c' {
  \clef treble
  \new Voice {
    e2. d4 e a,8 b c d e4 |
    c4 a e'2 c4 d ~ d c
    <<
      {
	\voiceOne
	r4 e2 dis4 e a2 gis4 |
	a4
      }
      \new Voice {
        \voiceTwo
        b,2
	\down \voiceOne
	c4 a gis a8 b \up \voiceTwo c d e4 \down |
	\voiceOne c4
      }
    >>
    e8 f g a b4 g e8 fis g a
    <<
      {
	\voiceOne
	b4 |
	gis4 c2 b4 ~ b8 a a4. b8 gis4 |
	a2 
      }
      \new Voice {
	\voiceTwo
	fis4 |
	e1 r |
	<<
	  e2
	  \new Voice {
	    \voiceFour
	    \shiftNote
	    cis2
	  }
	>>
      }
    >>
    r4
    <<
      {
	s4 s1 
	s1 e'2 e4 f ~ |
	f e d2 ~ d4 cis d2 cis1 |
	r2 r4 b b b c b ~ |
	b4 a8 g a4 b gis2 r4 e |
	e e f e ~ \voiceTwo e d8 c d4 e |
	c4 d e2 e4
	\voiceOne e' e e |
	\oneVoice f e ~ e d8 c d4 e
	<<
	  {
	    \voiceTwo d2 e c r r4 c |
	    c c d c ~ c8 b16 a b4\prallmordent c g |
	    g g a <b g> c2 c |
	    c r4 c c c d c ~ |
	    c8 b16 a b4\prallmordent c g |
	    g g a g ~ g8 f e d e f g e |
	  }
	  \new Voice {
	    \voiceOne r4 g' g g a g ~ g f8 e f4 g |
	    e f2 e4 d2 c4 d |
	    b c2 g'4 g g a g ~ |
	    g f8 e f4 g e f ~ f e |
	    d2 c4 d |
	    b4 c2 b4 c2 r |
	  }
	>>
	\voiceOne f,8 d e c d e d4 ~ |
	d8 c c4\pralldown b8 c ~ c b c2 |
	r4 g'4 ~ g g
      }
      \new Voice {
	\voiceOne
	e=''4 ~ e d c a8 b
	c d e4 c a ~ \voiceTwo a gis a r |
	\down \voiceOne
	a,1 |
	\up \voiceTwo s4 a'4 a2
	<<
	  {
	    \voiceFour
	    r4 e e e
	  }
	  \new Voice {
	    \voiceTwo
	    a1
	  }
	>>
	\voiceTwo f4 e ~ e \down \voiceOne d8 c d2 c4 d |
	b4 \up \voiceTwo e2 \down \voiceOne b4 \up \voiceTwo e4 r8 e d4 c |
	b8 a c4 b2 |
	\voiceOne r4 b' b b |
	c b ~ b a8 g a4 \voiceTwo b gis a |
	\down \voiceOne
	r4 g! a b ~ b c2 b4 |
	c2 c4 e,4 ~ e8 f g4 f e |
	g a
	<<
	  { d, e8 f g2 }
	  \new Voice { \voiceThree b,4 c d2 }
	>>
	c4 b d e d2 c4 f8 e f4 e ~ |
	e8 f g4 f e g a
	<<
	  { d,4 e8 f g2 }
	  \new Voice { \voiceThree b,4 c d2 }
	>>
	c4 <d b> |
	d4 e d2
	<<
	  { c2 c4. c8 a8 b c4 \shiftNote g s g2 g \shiftOff c1 }
	  \new Voice { \voiceThree e,4. f8 g2 f4 g \shiftNote g d \shiftOnnn d e \shiftOnnn d2 \shiftOff g1 }
	>>
	
      }
    >>
    a='16 b c d e4 d c b8.\prall c16 d8 a c4 b r8 a g e |
    f d e4 d
    <<
      { \clef alto c4 d2 e }
      \new Voice { \voiceTwo s4 \down \voiceOne g,2. g4 }
    >>
    a16 b c d e4 d c |
    b8.\prall c16 d8 a c4 b r8 a g e f d e4 |
    d4 \oneVoice r r \clef treble g'4 ~ g8 a16 b c d e f g4 f |
    e4 d8. e16 f8. e16 e4\pralldown |
    d8. c16 c4\pralldown
    <<
      {
	\voiceOne b8 c4 b8 c4 s s2 s1 s
	r2 r4 g4 ~ g8 \oneVoice a16 b c d e f g4 \voiceOne d |
	e2 d4 c |
	f4. e8 d4 c |
	b4 c ~ c8 b16 a b4\prallmordent |
	c4. b16 a g8 d'16 c b a g f |
	e4 c' d2 |
	e2. d4 |
	c2 b4 a16 g a b |
	\oneVoice c d e d e f g e f e d c b a g fis |
	g16 a b8 g e d'16 c b a gis8 e |
	\voiceOne b'2 r8 g8 a16 b c d |
	\oneVoice e8 g, c16 d e f g8 e, f16 g a b |
	c8 b16 a g fis g a b a g a b c d c |
	a b c a b c d e f e d e d c b a |
	\voiceOne b2 <d a> <c a> b4 a |
	gis a ~ a gis |
	a1 a a |
      }
      \new Voice {
	\voiceTwo \down
	r4 g=4 ~ \voiceOne g8[ a16 b] \up \voiceOne c[ d e f] g4 f |
	e d8. e16 f8. e16 e4\pralldown |
	d8. c16 c4\pralldown b4 e |
	\voiceTwo d c f4. e8 \down \voiceOne d4 \up \voiceTwo s2 g4 ~ |
	g c b g |
	a4. g8 f4 e |
	d g r2 |
	<g e>4. r8 r2 |
	r b4 a |
	<b gis>1 |
	<a e>2 r |
	s1*2 |
	e2 s |
	s1*3 |
	gis4 e ~ e gis |
	e2 d4 c |
	b2 d |
	<e cis>2. <f d>4 |
	e4 d cis d |
	<e cis>1 |
      }
    >>
    %% 2.
    <<
      {
	a4. bes16 a g f e d c8 c' |
	a1 |
	a4. bes16 a g f e d c8 c' |
	b1 |
	g8 a b c d c16\prall b a8 b16 c |
	b8 a16\prall g a8 fis g4 r |
	b8 a16\prall g a8 fis g4 r |
	g8 a b c d c16\prall b a8 b16 c |
	b8 a16\prall g a8 fis g4 r |
	c16 b a g a8 fis g4 f'16 e d c |
	b8 g c16 b a g fis8 d e fis |
	g4 g16 f! e d g2 |
	a1 |
	a2 ~ a8. e16 f g a b |
	c1 |
	c8. a16 b c d e <<{f2}\\{\voiceTwo a,2}>> |
	a4 c ~ c b8 a |
	c2 b8. b16 c d e f |
	g1 |
	f8 e ~ e d c4 b |
	d8 c ~ c b a4 gis |
	c8 b ~ b a g4 f |
	e d\prallmordent e2 |
	e4 e8 a ~ a a g4 |
	g8 f ~ f f e2 |
	f4 g a2 |
	gis4 gis8 c ~ c c b4 |
	b8 a ~ a a g4 gis |
	a4 b c2 |
	b4. d8 ~ d d |
	c4 ~ c8 b ~ b4 |
	c4. b8 ~ b b |
	a4 ~ a8 gis ~ gis4 |
	a8. d,16 e8 e ~ e16 f g8 |
	a8. b16 c8 c ~ c16 d b8 |
	e,8. f16 g8 g ~ g16 a b8 |
	b8. c16 d8 d ~ d16 e cis8 |
	d8. c16 b8 b8. c16 a8 |
	c8. b16 a8 a8. b16 gis8 |
	g!16 f g a f8 f16 e f g e8 |
	e16 d e f d8 d16 c d e c8 |
	b4. c16 d e f g e f e d c |
	b8 a c a b2 |
	cis4. cis8 d16 e f g a b c a |
	bes a g f e d f d e2 |
	e'16 d e f e8 e cis a e' a, |
	a16 g a b a8 a fis d a' d, |
	d'16 c d e d8 d b g d' g, |
	g16 f g a g8 g e c g' c, |
	g''2 a4 g |
	f e d2 |
	g2 a4 g |
	f e d2 |
	c1 |
	g2 f4 e |
	d2 r4 g4 ~ |
	g a g2 |
	f4 e d2 |
	\times 2/3 {g4 c d} |
	
      }
      \new Voice {
	\voiceTwo
	f='2. r8 e |
	f4. e16 d \down \voiceOne c bes a g f4 |
	\up \voiceTwo
	f'2. r8 e |
	g4. f16 e \down \voiceOne d c b a g4 |
	\up \voiceTwo r4 g' ~ g fis |
	g d d c |
	d2 d4 c |
	d g2 fis4 |
	g d d c |
	\down \voiceOne r8 d r d c16 b a g a8 fis |
	r8 d'4 cis8 d4 d16 c b a |
	d4 s \up \voiceTwo e8 d16 c d8 e |
	f2. f4 |
	e2 r |
	a g |
	a8. a16 r4 \down \voiceOne c,8. a16 b c d e |
	\up \voiceTwo f4 e ~ e d |
	e2 r |
	r8 g c b c d b c |
	a g ~ g b a4 g |
	\voiceFour a8 g ~ g g e4 e |
	\voiceTwo e8 g ~ g f e4 d |
	b r b2 |
	cis4 cis8 f ~ f f e4 |
	e8 d ~ d d cis2 |
	c!4 e ~ e d |
	e2 r |
	e e |
	e4 g ~ g fis |
	g2 a4 g |
	f d e g |
	fis e d b |
	<e cis>8. s16 s4 s2 |
	s1 s |
	d8 g4 g8 e4 |
	f8 e4 e8 e4 |
	e8 c4 c8 c4 |
	c8 a4 a8 a4 |
	s1 s s \down a |
	\up r4 e'2 e4 |
	d4 d2 d4 |
	r4 d2 d4 |
	c4 c2 c4 |
	e'2 f4 e |
	d c b2 |
	e2 f4 e |
	d c ~ c b |
	r4 g ~ g a |
	e2 d4 c ~ |
	c b c e ~ |
	e f e2 |
	s1 |
	s2 |
      }
    >>
    %% 3.
    <<
      {
	\oneVoice e'4 f d e g f e2 d4 g,,4 c d4 |
	e f d e g f e2 d4 g c d |
	e f8 g a4 g e f e2 d4 g,, c d |
	e f8 g a4 g e f e2 d4 e' d c |
	b2 a4 e d c b2 r4 e' d c |
	\voiceOne b2 a4 gis2 \oneVoice r4 b d c b2 a4 |
	gis2 a4 b d c \voiceOne b2 a4 gis!2 a4 |
	\oneVoice e' g f \voiceOne e2 d4 c b a \oneVoice e'8 f g4 f |
	\voiceOne e2 d4 c b a c b2 a gis4 |
	a2. d,8 e f!4 d a'2. a8 b c4 a |
	b4 c8 d e4 d2\prallprall b4 d4 c2\prallmordent a4 a'2 |
	g4 e8 f g f e d c b a c b c d c b a g! f e fis g a |
      }
      \new Voice {
	\down \voiceOne c='2 b4 c2 a4 r c b s s b |
        c2 b4 c2 a4 r c b c2 b4 |
	r4 c c c2 a4 r4 c b s a b |
	r c2 c a4 r c b g2 a4 |
	gis2 a4 \up \voiceTwo c4 b a \down \voiceOne gis2 a4 g2 a4 |
	\up \voiceTwo e'4 d c b2 \down \voiceOne a4 gis2 a4 b d c |
	b2 a4 gis2 a4 \up \voiceTwo b4 d c e2 d4 |
	\down \voiceOne c4 b2 \up \voiceTwo e4 g f e d2 \down \voiceOne c4 b2 |
	\up \voiceTwo e8 f g4 f e d2 e8 f g4 \voiceFour f e d \voiceTwo r |
	<e cis>2. s2. <e cis> \down \voiceOne e2 fis4 |
	\up \voiceTwo g2. \down \voiceOne d f4 e2 e d4 |
	e1. e2. ~ e2 e4 |
      }
    >>
    \oneVoice
    \clef treble b'8 c d e f e d e f e d c b c d c b a g a b c a b c a b c d e f e d c b a g fis g a b c b a b a g fis gis a b a gis a fis gis gis2\prallmordent |
    bes4 a2 a4 f2 f4 d2 |
    a'2. ~ a4 e2 e e4 |
    g2 g4 a2 a4 |
    g2. ~ g4 r2 r2. |
    <<
      {
	\voiceOne
	a4 b a b c a b2 g4 |
	fis2.\prallprall g2 fis4 g2. |
	g1.
      }
      \new Voice {
	\voiceTwo
	d1. ~ d2. |
	s1. s2. |
	d4 e d e f d
      }
    >>
    \oneVoice
    e2 c4 g' a b
    c d b c2 g4
    %% 4.
    \voiceOne c1. ~ c2. |
    d1. ~ c2. |
    <<
      {
	\voiceOne b2. c d |
	e2 d4 c b a b c a |
	g a b a8 b c4 b g a f |
	e f g f8 g a4 f e f d |
	c d e d8 e g4 d f e c |
	e d b d c a c b gis |
	a8 gis a b c d c b c d e f e d e f g a |
	g f g a b c b a b c d e f e d c b a |
	g4 g'8 f e d c4 f8 e d c d c b a b c |
	b4 d8 c b a g4 bes8 a g f g f e d e f |
	e d c d e f g a b c b a g f e d cis b |
	a'2. c b |
	c b a |
	g8 a b g a b c d e c d e fis g e fis fis4\prallmordent |
	g4 f8 e d c b a b c d e d b c d e f |
	e c d e f g a e fis g fis e d c b a gis fis |
	gis a b c d e f e d c b a gis fis gis a b a |
	gis fis e fis gis a b a gis fis gis4\prallmordent |
	\parenthesize a4 e ~ e8 f g e |
	a1 |
	a |
	f8. e16 f g f e d ees d c bes a bes g |
	a bes c d e f d e f g a f g4\prallmordent |
	a8 d,16 e f g a b! c d b cis cis4\prallmordent |
	d4 f e d |
	cis2 r16 e, f g a b cis d |
	e f e d cis b a gis a b cis d e d cis b |
	cis\breve |

      }
      \new Voice {
	\voiceTwo g='2. g \times 3/2 {a4 a} |
	c2 s4 s1. |
	\down \voiceOne e,2 d4 ~ d c ~ c e2 c4 |
	c2 b4 ~ b a ~ a c2 a4 |
	a2 a4 ~ a g ~ g a2 a4 |
	s1. s2. s1. s2.
	\clef alto e'1. d2. |
	e1. f2. |
	g2. e d |
	c1. a2. |
	\up \voiceFour e'2. a g |
	g g s |
	\down \voiceOne e1. c2. |
	d2. g,1. |
	e1. e2. |
	e1. ~ e2. |
	e1. |
	\up \voiceTwo cis'2 s |
	f2 e4 d |
	cis d e2 |
	s1*3 |
	<a f>1 |
	<a e> |
	s |
	<a e>\breve |
      }
    >>
    
  }
}

lower = \relative c' {
  \clef bass
  \new Voice {
    R\breve |
    a2. g4 a d,8 e f g a4 |
    \voiceTwo
    g4 e a f e f2 e4 |
    a4
    <<
      {
	\voiceOne r4 e' d c2 b |
	r4 a2 gis4 a a8 b \up \voiceTwo c d b4 |
	\voiceOne \down
	a2
      }
      \new Voice {
	\voiceTwo c2 g4 r e2 dis4 |
	e a,8 b c d e4 c a e'2 |
	<<
	  {
	    e2 
	  }
	  \new Voice {
	    \voiceFour
	    \shiftNote
	    a,2
	  }
	>>
	
      }
    >>
    r4
    <<
      {
	<<
	  {
	    \voiceOne
	    c='4 ~ c b c c
	  }
	  \new Voice {
	    \voiceThree
	    g4 ~ g \shiftNote g g a
	  }
	>>
	a4 gis a4. b8 c d e4 c d8 e |
	\up \voiceTwo f8 g a4 f d |
	a' \voiceFour \shiftNote e \shiftNote f2 \down \voiceOne a,2 r4 a |
	\voiceTwo a4 a c b ~ b a8 g a4 g ~ |
	g \voiceOne b c \voiceThree gis \voiceOne b2 a4 g! ~ |
	\voiceOne g a2 gis4 a g2 f8 e \clef alto a4 g g c c \up \voiceFour \shiftNote e2 \down \voiceOne d8 c |
	\voiceTwo d4 e a, g' ~ g f8 e f4 g |
	c,4. c8 f4 c c c d c ~ |
	c b8 a g2 g4 g a g |
	g f8 e f4 g <g c,>2 r4 c4 |
	c c d c ~ c b8 a g2 |
	g4 g a g ~ |
	g f8 e f4 g \clef bass c,2 c4 c |
	d c c b8 a |
	b4 c g2 c1 |

      }
      \new Voice {
	\voiceTwo
	c=4 ~ c \shiftNote g' c, f ~ f e f2 e r4 d ~ |
	d cis d d8 e |
	<<
	  {
	    \voiceOne
	    f8 g a4 f d
	    e1
	  }
	  \new Voice {
	    \voiceTwo
	    a,2 r a1
	  }
	>>
	\voiceTwo
	s1 s |
	s4 e= a e e e f e ~ |
	e d8 c d4 e <e a,> r r2 |
	r2 e4 a ~ a <b gis>8 <a fis> e4 r |
      }
    >>
    \clef alto \oneVoice r4 c='2 c4 |
    d16 e f g a4 g f e8.\prall f16 g8 d f4 e |
    r8 d8 c a b g
    <<
      {\up \voiceTwo c2 b4 c2}
      \new Voice {\voiceTwo r4 \clef bass r2 c,2 ~ c4}
    >>
    \down \oneVoice
    c4 d16 e f g a4 |
    g4 f e8.\prall f16 g8 d f4 e r8 d c a |
    b g
    <<
      { \voiceTwo c4 ~ c b }
      \new Voice { \voiceOne s4 d2 }
    >>
    \clef alto
    \oneVoice r4 c'4 ~ c8 d16 e f g a b |
    c4 b a g |
    f e \voiceOne d2 |
    s4 c,4 ~ c8 d16 e f g a b |
    c4 b a g |
    \clef bass
    <<
      {
	f4 e d c' b g a c \voiceThree b \voiceOne c ~ c b |
	c8 \clef alto \oneVoice a16 b c d e f g4 e |
	\clef bass \voiceOne d c r2 |
	r4 e4 d d |
	c1 |
	c4. b16 a g8 d'16 c b a g f |
	\oneVoice e8. f16 g a b c b a g f e d c b |
	a b c d e f g a b c d e f4 ~ |
	\voiceOne f e d c |
	<e b>1 |
	gis,16 a b8 gis e b'2 |
	c2. d4 |
	r c d b |
	c a ~ a8 b c d |
	s1*5 |
	a4. a8 e4 a |
      }
      \new Voice {
	\voiceTwo
	r4 g=,4 ~ g8 a16 b c d e f  g4 e d c g' a g2 |
	<g c,>8 s s4 s2 |
	r4 c,4 ~ c8 a16 b c d e f |
	g4 e f g |
	<g c,>1 |
	<g c,>4. r8 r2 |
	s1 s |
	a1 |
	e |
	r2 <g e>4 <f d> |
	<g c,>2. r4 |
	c,2 g' |
	f1 |
	\oneVoice e16 f g a b c d c b a g f e d c b |
	a b c d e a, b c d e f g a, b c d |
	e b c d e f g a b a g f e d c b |
	a b cis d e d cis b a g f e d8 d' |
	cis a d f e a f d |
	\voiceTwo <e a,>1 |
	
      }
    >>
    %% 2.
    <<
      {
	c='2. s8 c |
	c4. s8 s2 |
	c2. s8 c |
	d4. s8 s2 |
	r4 d ~ d d |
	d a b8 a16\prall g a8 fis |
	b4 a b8 a16\prall g a8 fis |
	b4 r s2 |
	d4 a b8 a16\prall g a8 fis |
	\voiceThree b4 a s2 |
	s2 a4 s |
	s1 |
	\voiceOne <d a>2 a4 d ~ |
	d cis8 b cis4 d |
	e f ~ f e |
	<f c>2 s |
	s2 a,2 ~ |
	a4 gis8 fis gis4 a |
	b8 e ~ e d c b d e |
	c r r4 e \up \voiceTwo e |
	f8 e ~ e d c4 b |
	\down \voiceOne a8 g ~ g a c4 a |
	gis a ~ a gis\prallmordent |
	a1 |
	a2 s |
	s4 b c r |
	b b8 e ~ e e d4 |
	d8 c ~ c c b4 |
	e,8 c' ~ c c d4 c2 |
	d4 b a |
	e' d2 |
	c4 g b |
	c b2 |
	a4 c b \up \voiceTwo c |
	e d c e |
	d g f e |
	\down \voiceOne
	d8 d4 d8 c4 |
	c8 c4 c8 b4 |
	b8 a4 a8 g4 |
	g8 f4 f8 e4 |
	gis2. a4 |
	gis4 a2 gis4 |
	a1 |
	e |
	r4 cis'2 cis4 |
	a4 a2 a4 |
	r4 b2 b4 |
	g4 g2 g4 |
	\up \voiceFour \shiftNote c'2 \shiftNote c4 \shiftNote c |
	s1 |
	\shiftNote c2 \shiftNote c4 \shiftNote c |
	s1 |
	\down \voiceOne r4 e,4 ~ e f |
	s1 s s s2 s4 b,4\prallmordent |
	c2 |
	

      }
      \new Voice {
	f=2. r8 a |
	f4. r8 s2 |
	f2. r8 a |
	g4. r8 s2 |
	s4 g ~ g d' |
	g, d r2 |
	g4 d r2 |
	g4. a8 b c d4 |
	g, d r2 |
	g4 d r2 |
	g4 e d r |
	b'8 g a b c b16 a g8 f16 e |
	d4. e8 f4 d |
	a'1 |
	a4. b8 c2 |
	f,1 |
	a4. g8 f2 |
	e1 |
	e4. g8 c, g' ~ g e |
	f c ~ c g' a4 e |
	d8 e ~ e16 f g8 \voiceTwo c,8. d16 e4 |
	a,8 e' ~ e f c4 d |
	e f e2 |
	a,2 c! |
	d a4 a8 a' ~ |
	a8 a g4 g8 f ~f f |
	e2 g! a e4 r r |
	b'4 ~ b8 a ~ a a |
	g2 f4 e f g e e d c d e |
	<e a,> a g f e g a e g e d a' |
	b8 g4 g8 a4 |
	f8 a4 a8 e4 |
	e8 f4 f8 c4 |
	c8 d4 d8 a4 |
	e1 |
	e |
	a2. b4 |
	cis d cis a |
	s a'2 a4 |
	fis4 d2 d4 |
	r g2 g4 |
	e c2 c4 |
	c'16 b c d c8 c a f c' c, |
	f d e c g'16 f g a g8 g |
	e c c'16 b a g f g a b c8 c, |
	\clef alto f16 g a b c d e f g f g a g8 g |
	e c c16 b c d c8 c a f |
	\clef bass c' c, d e f d e c |
	g'16 f g a g8 g e c c'16 b c d |
	c8 c a f c' c,16 d e f g a |
	b8 g c4. b16 a g4 |
	<g c,>2 |
      }
    >>
    %% 3.
    <<
      {
	\voiceTwo g=2 g4 g2 a4 g2 g4 g2 g4 |
	g2 g4 g2 a4 g2 g4 g a f |
	g2 f4 g2 s4 g2 s4 s2. |
	g2 f4 g2 a4 s2. s |
	s a2 a4 s2. s |
	e2. s d2 e4 e2. s d2 e4 e2. b'2 s4 g2 s4 \voiceOne <c g>2.
	s s <c g>2 r4 s2. |
	g4 b8 c \up \voiceTwo d4 c b \down \voiceOne b a fis8 g a4 f d2 |
	\clef alto \oneVoice  r4 \voiceOne cis'8 d e4 e2 fis4 d2 s4 s2. |
	s \shiftNote c4 a2 \shiftNote b2. c2 a4 |
	\shiftNote b2 c4 b2 s4 |
	d2 c4 a2 c4 d2 e4 ~ e d2 |
	c2. d e2 d4 c d2 |
	\clef bass b1.
	a1. ~ a2. |
	a1. ~ a2. |
	c1. |
	e,2 e4 g2 g4 a2 a4 |
	fis2. s s |
	a4 b a b c a b2 g4 |
	b c b c d b c2. |
	e2 d4 c2 b4 c2. |
      }
      \new Voice {
	\voiceFour \shiftNote c=2 s4 \shiftNote c2 \shiftNote d4 \shiftNote c2 s4 \shiftNote c2 s4 |
	\shiftNote c2 s4 \shiftNote c2 \shiftNote d4 \shiftNote c2 s4 \shiftNote e4 \shiftNote f \shiftNote d |
	\shiftNote c2 s4 \shiftNote e \shiftNote c \shiftNote d \shiftNote c2 g'4 e f d |
	\shiftNote c2 s4 \shiftNote e c \shiftNote d c2 g'4 c, b a |
	e'2 a,4 \shiftNote <e' a,>2 \shiftNote <e a,>4 e2 a,4 c b a |
	\shiftNote gis2 a4 e' d c \shiftNote b2 \shiftNote a4 \shiftNote gis2 a4 |
	b d c \shiftNote b2 \shiftNote a4 \shiftNote gis2 a4 \shiftNote e' g f |
	\shiftNote e2 d4 c2. e4 g f e2 d4 |
	c2 r4 e8 f g4 f \shiftNote e2 \oneVoice b4 ~ b8 c d4 \voiceFour e |
	<e a,>4 s2 s2. s <c' a> |
	g2 c4 b g2 a2. a4 f2 |
	e2. a2 a4 gis2 a4 e2 c'4 |
	g2 a4 f2 a4 g2. g |
	a2 f4 ~ f g2 e g4 a d,2 |
	e2 b4 d e2 |
	f2. ~ f4 d2 ~ d2. |
	%\times 3/2 {f2. d2.} |
	f4 d2 d4 c2 c4 a2 |
	e'2 c4 ~ c f2 |
	c2 c4 e2 e4 f2 f4 |
	d2. g2 fis4 g2. |
	d2. g2 d4 g2 g4 |
	g1. c,2. c'2 g4 a2 g4 <g c,>2. |
      }
    >>
    %% 4.
    <<
      {
	\clef alto
	\up \voiceTwo e='4 f g a bes g a2 g4 |
	fis2. g2 f4 e \down \voiceOne d c |
	\up \voiceFour \shiftNote d2. \shiftNote e \shiftNote f |
	\shiftNote g2 \down \voiceOne c,4 e d c d e c |
	b2 b4 ~ b s s b2 a4 |
	\clef bass
	g2 s s g f4 |
	e2 e4 ~ e s s s2 s4 |
	b2. e1. |
	e2. a c |
	\voiceFour b1. a2. |
	b c1. |
	d1. b2. |
	g e f |
	\up \voiceTwo c'! e e |
	e e s |
	\down \voiceFour b s1. |
	b2. \clef bass d,1. |
	c1. c2. |
	b1. c2. |
	b2 a4 b2 s4 |
	\voiceOne e4. b8 cis d e cis |
	s1*2 |
	d2. e4 |
	f g a bes |
	a4. g8 a4 e |
	r16 a f g a g a b cis d e cis d4\prallmordent |
	<a e>\breve |
	<a e>\breve |
      }
      \new Voice {
	\voiceTwo c='4 d e f g c, f2 e4 |
	d2 c4 b a g c2. |
	g=2 f4 e d c f e d |
	<c' c,>2. c2 c4 g2 a4 |
	e2 g4 ~ g a ~ a e2 f4 |
	c2 e4 ~ e f ~ f c2 d4 |
	a2 c4 ~ c b ~ b a2 a4 |
	g2. a e |
	a1. a'2. |
	e g f |
	e a1. |
	g1. g2. |
	c,1. d2. |
	\clef bass a8 b c d e d c b a b c d e f g f e d |
	c d e f g f g a b c d e f e d c b a |
	e2. a1. |
	g2. g,1. |
	a1. a2. |
	e1. ~ e2. |
	e1. |
	a1 |
	d8 d,16 e f g a b c a b c d e f d |
	e f e d cis b a b cis d b cis cis4\prallmordent |
	<a d,>1 |
	d |
	d |
	d2 r |
	a\breve |
	a\breve |
	
      }
    >>
    
  }
}

\score {
  \new PianoStaff <<
    \set PianoStaff.instrumentName = ""
    \new Staff = "upper" << \global \upper >>
    \new Staff = "lower" << \global \lower >>
  >>
   \layout { }
   \midi { }
 }
