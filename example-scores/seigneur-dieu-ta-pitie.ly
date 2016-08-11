\version "2.18.2"

% Seigneur Dieu ta pitié
% for Guillaume Costeley's 19-tone keyboard

#(set-global-staff-size 15)
#(set-default-paper-size "a4" 'landscape)

up = { \change Staff = "upper" }
down = { \change Staff = "lower" }

shift= { \once \override NoteColumn.force-hshift = #0.7
	      \shiftOnn }

global = {
  \once \override Staff.TimeSignature #'stencil = ##f 
  \time 4/2
  s1*108
  \bar "||"
  s1*92
  \bar "||"
  s1*58
  \bar "|."
}

bass = \relative c' {
  \clef bass
  a1 a1 d,1 bes'1.  a2
  %co v3,
  g1 r2 g2 bes2 bes2 c2 c2 g1 r2 c,2 g'2 g2 aes2.  g4 f2 f2 c1 r1
  r\breve r1 r2 f2 f1 bes,1 ges'1.  f2 ees1 r2 bes2 ges'2 ges2 aes2
  aes2 ees1 r2 aes,2 ees'2 ees2 fes2.  ees4 des2 des2 aes1 r2 des2
  fes1.  ces2 des2 ais2 ces1 fes1.  fes2 fes2 fes2 ges2 dis2 fes1
}


tenor = \relative c' {
  %\clef alto
  s1*4 r1 d1 d1 g,1 ees'1.  d2
  %co v2,
  c1.  b2 ees2 ees2 f2 f1 e2 f2 f,2 c'2 c2 des2.  c4 bes2 bes2 f1 r2
  c'2 des2.  c4 bes2 bes1 a2 bes1 bes1 ees,1 ces'1 bes1 aes1 r2 g2
  ces2 ces2 des2 des2 c2 c2 des1 ces1 fes2 ees2 des1 ces1 aes1 r2
  aes2 aes2 aes2 ais2 ais2 aes1
}
   
alto = \relative c'' {
  %\clef alto
  R1*10 g1 g1 c,1 aes'1.  g2
   %co v1,
  f1 r2 e2 aes2 aes2 bes2 bes1 a4 g4 a4 g4 f4 ees4 f1 des1 r2 f2 ges2
  ges2 f2 f2 ees\breve.  ees1 ees1 aes,1 fes'1 ees1 des2 fes2 fes4
  ees4 fes4 ges4 aes2 ges2.  fes4 fes1 ees2 fes2 ces2 ces2 ces2 des1.
  dis2 ces1
}

treble = \relative c'' {
  \clef treble
  s1*14 r1
  c1 c1 f,1 des'1.  c4 bes4 c1 bes1 r2 bes2 des2 des2 ees2 ees2 d1 r2
  bes2 aes1 g2 g2 ces2 ces2 bes2 bes2 aes\breve r2 aes2 aes1.  aes2
  ces2.  bes4 aes2 ais2 ges1 fes\breve.  r1
  %co v1a
}

trebleA = \relative c' {
  r2 fes2 fes2
   %co v2a,
   %co v3a,
   %co v4a,
  fes2 aes1 ais1 ges2 ges2 gis2.  ais4 bis1 ais1 aes2 aes2 ais2 ais1
  gis2 fis1 eis1 eis1 gis2.  gis4 fis2 ais2 gis1 r1.  gis2.  gis4
  fis2 eis2 eis2 dis1 r\breve r2 gis2 cis1 bis1 r1.  gis2 b1
  ais\breve r2 gis2 cis1 fis,2 b1 ais2 gis1 gis1 gis1 gis2 ais2 b1
  ais1 gis1 r1.  gis2 bis2.  cis4 dis2 dis2 gis,\longa r2 cis2 cis2
  bis2 cis2 cis2 ais1 gis2 r\breve cis2 cis2 bis2 cis1.  cis2 bis2
  ais4 aes4 ais1 aes\breve
  %co v1b
}


altoA = \relative c' {
  r2 ces2 fes2.  fes4 fes2 ges2 dis1 r2 dis2 gis2
  gis2 eis2 eis2 eis1 r2 eis2 eis2 eis2 cis2 cis2 cis1 r2 cis2 eis2.
  eis4 dis2 fis2 eis2.  dis8 cis8 dis2 dis2.  dis4 cis2 bis2 ais2
  gis1 r1.  cis2 fis1 eis1 r2 cis2 gis'1 eis2.  dis8 cis8 dis1 r2
  dis2 fis1 cis2 cis2 eis\breve dis2 fis1 fis2 dis1 eis\breve eis1
  dis2 eis2 fis1 eis\breve dis1 r2 eis2 fis1 eis1 eis1 dis1 cis2 eis2
  eis2 cis2 dis2 dis2 eis4 dis4 cis4 bis4 cis2 r1 fis2 fis2 fis2
  eis2.  fis4 dis1 cis2 gis'2 gis2 gis2 fis2 eis1 dis2 eis\breve
}


   
tenorA = \relative c' {
  r2 aes2 ces2.  ces4 des2 dis2 ais2 ais2 bis2.
  cis4 dis1 cis1 bis2 bis2 cis2 cis1 bis2 ais1 gis\breve r2 gis2 b2
  fis2 cis'2 cis1 bis4 ais4 bis2 r\breve cis2.  cis4 bis2 ais4 gis4
  gis1 fis2 gis\breve r2 gis2 cis1 bis1 r\breve r2 ais2 cis1 gis2
  gis2 b2.  cis4 dis2 cis1 bis2 cis\breve r1 gis1 ais2 bis2 cis1 cis1
  bis1 gis1 ais2.  bis4 cis2 cis2 gis2 cis2 cis2 bis2 cis2 cis2 ais1
  gis1 r1.  cis2 cis2 ais2 ais2 fis2 gis1 gis1 r2 eis'2 eis2 eis2
  dis2 cis2 ais1 bis\breve
}



   
bassA = \relative c {
  r2 fes2 fes2 fes2 ais2 dis,2 dis2 dis2 r2 gis2 gis2
  gis2 ais2 ais2 eis1 r2 ais,2 eis'2 eis2 fis2 fis2 cis\breve.  r1.
  cis2 gis'2.  fis4 gis2 eis2 dis1 r1 gis2.  gis4 fis2 eis2 dis1
  cis\breve r1.  cis2 gis'1 gis2.  fis8 eis8 dis2 dis2 fis1 cis\breve
  r2 b1 fis'2 gis1 cis,\breve.  r\breve cis1 eis2 fis2 gis1 eis1 dis1
  r2 cis2 eis2.  fis4 gis2 gis2 cis,1 r\breve r2 fis2 fis2 fis2 eis2
  fis2 dis1 cis1 r1.  cis2 cis2 cis2 dis2 eis2 fis1 eis\breve
}


trebleB = \relative c'' {
  ces\breve fes,1 ces'1 cis2
   %co v2b,
  ais1 aes4 ges4 aes1 ais2 cis1 bis2 ais4 gis4 fis2 eis1 r2 ais2 ais1
  ais1 fis2 gis2 ais\breve r2 gis2 fis2 ais2 b1 gis1 ges\breve
  fis\breve r\breve dis1 gis1 ges2 b1 ais2 gis1 fis1 r2 gis2 ais1
  ais1.  b2 gis1 fis2 fis2 fis2 fis2 gis2 b1 ais4 gis4 ais1 fis1 b1
  gis1 fis\breve fis1 fis2 gis2 a1.  b2 cis\breve gis1 cis2 cis2 a2
  a2 d1 cis1 r1 cis1 cis2 cis2 cis2.  b4 a2 a2 b1 gis1 r2 gis2 a2 a2
  fis2 b2 gis1 r2 a2 gis1 gis1 fis2.  e4 dis1 r2 gis1 gis2 cis1 cis2.
  b4 a1 gis2 gis1 gis2 cis1 ces\breve r2 cis1 a1 a2 b2 cis1 b1 ais4
  gis4 ais1
  %co v1c
}

altoB = \relative c' {
  R1*4 r1 eis\breve ais,1 eis'1 fis2
   %co v3b,
  dis1 des4 ces4 des1 dis2 fis1 eis2 dis2 b2 ais1 r2 cis2 b2 dis1
  cis2 b4 ais4 dis1 cis2 dis1 ais1 b1 ais2 bis2.  cis2 b8 ais8 b1
  ais1 r2 gis2 dis'1 dis1.  dis2.  cis4 cis1 bis2 cis1 r2 cis2 dis1
  dis2 e2 cis1 b2 dis2 dis2 e2 fis2 dis2 cis\breve r2 b2 e1 dis1
  cis\breve r1 cis1 cis2 dis2 e1 eis2 fis1 eis2 fis\breve.  r2 cis2
  fis2 fis2 e2 e2 a1 gis2 a1 fis2 fis2 gis1 fis1 eis2 fis1 r2 dis2
  eis2 eis2 fis2 fis2 cis1 r2 e2 dis2 cis1 bis4 ais4 bis2 bis2 cis2
  cis1 cis2 fis1.  fis1 eis4 dis4 eis1 cis1 dis1 r2 gis1 e1 cis1 fis4
  e4 dis4 cis4 e2 d1 cis\breve
}

tenorB = \relative c' {
  R1*10 ais\breve dis,1 ais'1 b2 gis1 ges4 fes4
  ges1 gis2 b1 ais2 gis1 e1 dis\breve.  dis1 e1 dis2 gis1 fis2 e1
  dis2 gis1 fis2 b1.  ais2 gis1 fis\breve r2 gis2 gis1 ais1 b1 gis1
  fis\breve.  e1 e1 b'1 fis\breve fis1 fis2 gis2 a1.  b2 cis\breve r1
  fis,1 b2 b2 a2 a2 d1 cis1 r2 fis,2 cis'2 cis2 d1 b1 cis1 cis1 r2
  fis,2 b2 b2 cis2 cis2 fis,1 r1 e1 fis2 fis2 gis1 gis1 cis,\breve r2
  fis1 fis2 cis'1 cis2.  b4 a1 gis\breve cis1 a1 fis1 b2 a2 b1
  fis\breve
}


trebleC = \relative c'' {
   %co v2c,
   %co v3c,
   %co v4c,
  a1 gis1 b1 b2 a2 gis\breve cis1 cis2 b2 a2 a2 gis1 r2 a2 b2.  b4 b2
  a2 gis1 r2 cis2 cis2 b2 a2 a2 gis2 gis2 cis2.  b4 ais2 gis2 fis4
  eis4 fis4 gis4 ais1 r2 gis2 cis2.  b4 ais2 fis2 ais2 b2 cis1 r2
  fis,2 b2 b2 ais2 gis2 fis1 eis2 eis2 ais1 r2 gis2 b2.  ais8 gis8
  fis2 ais2 gis4 fis4 fis4 eis8 dis8 eis1 dis2 r4 ais'2 eis2 gis2
  dis4.  eis8 fis4.  gis8 ais2 b4.  ais8 gis8 ges8 gis2 ges2 b2 ais2
  gis2 ges2 gis1 ges2 gis2 r4 ais2 eis2 gis2 dis4.  eis8 fis4.  gis8
  ais2 b4.  ais8 gis8 ges8 gis2 ges2 b2 ais2 gis2 ges2 gis1 ges2
  gis\breve
}

altoC = \relative c' {
  fis1 eis1 fis1 gis2 fis2 eis\breve a1 a2 gis2 fis2 fis2 eis1 r2
  fis2 gis2.  gis4 gis2 fis2 eis1 r2 a2 a2 gis2 fis2 fis2 eis\breve.
  r2 cis2 fis2 fis2 eis2.  dis4 cis1.  cis2 fis2.  fis4 eis2 eis2
  dis2 dis2 fis2 fis2 fis2 eis2 dis1 des1 eis1 eis1 dis1 dis2 fis2
  eis2 dis1 des2 dis2 fis2 cis2 cis2 b2 b2 ais4 dis4.  cis8 b8 ais8
  gis4 b2 ais8 gis8 ais4 dis4.  cis8 b8 cis8 dis8 eis8 fis4 dis2.
  dis4 b4 cis4 dis1 dis2 fis2 cis2 cis2 b2 b2 ais4 dis4.  cis8 b8
  ais8 gis4 b2 ais8 gis8 ais4 dis4.  cis8 b8 cis8 dis8 eis8 fis4
  dis2.  dis4 b4 cis4 dis1 bis\breve
}


tenorC = \relative c' {
  cis1 cis1 dis1 e2 cis2 cis\breve e1 e2 e2 cis2 cis2 cis1 r2 cis2
  e2.  e4 e2 cis2 cis1 r2 e2 e2 e2 cis2 cis2 cis1 r2 gis2 cis2.  b4
  ais2.  gis4 fis2 fis2 cis'2.  b4 ais2 gis2 fis4 gis4 ais1 gis4 fis4
  gis2 ais2 b1 dis1 cis2 cis2 ais1 ais1 r2 ais2 cis2.  b4 gis2 gis2
  b2 fis2 gis2 b2 ais1 dis,2 r2 ais'2 eis2 gis2 dis4.  eis8 fis4.
  gis8 ais4 gis8 ais8 b8 cis8 dis2 cis4 dis2 r4 dis2 cis2 b4 ais2
  gis2 ais1 gis4 b2 ais4 ais2 eis2 gis2 dis4.  eis8 fis4.  gis8 ais4
  gis8 ais8 b8 cis8 dis2 cis4 dis2 r4 dis2 cis2 b4 ais2 gis2 ais1
  gis\breve
}

bassC = \relative c {
  fis1 cis1 b1 e2 fis2 cis\breve a1 a2 e'2 fis2 fis2 cis1 r2 fis2 e2.
  e4 e2 fis2 cis1 r2 a2 a2 e'2 fis2 fis2 cis\breve r2 cis2 fis2.
  eis4 dis2 dis2 cis1 r2 cis2 fis2.  eis4 dis2 dis2 cis1 b1 b2 b2
  fis'2 cis2 dis1 ais\breve r1 r\breve r\breve r2 dis2 ais2 cis2
  gis4.  ais8 b4.  cis8 dis4.  eis8 fis4 gis4.  fis8 e8 dis8 e2 dis2
  gis2 fis2 gis2 dis2 e2 dis1 gis,2 dis'2 ais2 cis2 gis4.  ais8 b4.
  cis8 dis4.  eis8 fis4 gis4.  fis8 e8 dis8 e2 dis2 gis2 fis2 gis2
  dis2 e2 dis1 gis,\breve
}




\score {
  \new PianoStaff <<
    \set PianoStaff.instrumentName = ""
    \new Staff = "upper" << \global \\
			    { \voiceOne \treble \trebleA \trebleB \trebleC} \\
			    { \voiceTwo \alto \altoA \altoB \altoC } >>
    \new Staff = "lower" << \global \\
			    { \voiceOne \tenor \tenorA \oneVoice \tenorB \voiceOne \tenorC } \\
			    { \voiceTwo \bass \bassA s1*92 \bassC } >>
  >>
   \layout { }
}

