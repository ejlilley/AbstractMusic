\version "2.18.2"

% Carmina Chromatico (from Prophetiæ Sibyllarum)
% A madrigal by Orlando de Lasso.

#(set-global-staff-size 15)
%#(set-default-paper-size "a4" 'landscape)

global = {
  \time 4/2
  \key c \major
  s\breve*25
  \bar "|."
}


bassus = \relative c {
  \clef bass
  r1 c1.  g2 g1 b\breve cis1.  e2 e1 fis1 fis1 g2 c,1 f2 bes,1 d2 ees2
  d1 g1 c,\breve e\breve a,1 d1 r2 g1 g2 e2 e2 fis1 b,1 e1 gis1 a1 d,1
  g\breve c,2 c2 f1 d2 g2 ees2.  bes'4 bes2 f2.  a4 g2 e2 a2.  d,4 g2
  f1 bes,\breve f2 a2 c2.  b4 a1 g\breve
}



tenor = \relative c' {
  \clef tenor
  g1.  g2 g\breve fis\breve cis'1.  b2 b1 r2 a2 a1 b2 c1 a2 bes1 a2 g2
  a1 b1 c\breve b\breve cis1 d1 g,1 b1.  b2 ais2 ais2 b1 b1.  b2 cis1
  d2 d1 c2.  b8 a8 b2 c2 c2 c1 d2 d2 ees2.  d4 d2 c2.  c4 d2 e2 cis2.
  d4 bes2 a1 bes1.  bes2 a2 a2.  g4 g1
  fis2 % original f natural
  g\breve
}

   

altus = \relative c' {
  \clef alto
  r1 c1.  b2 b1 b\breve gis1.  e'2 e1 cis1 cis2 d1 e1 c2 d1 d2 c2 d1
  d1 g\breve e\breve.  fis1 g1 d1 e1 cis1 dis1 e1 dis2 e1 a,2.  g4 a2
  b2 e2 d1 e2 e2 f1 fis2 g2 g2.  f4 f2 f2.  e4 g2 gis2 a2.  fis4 g2
  c,1 d\breve c2 c2 c\breve b\breve
}

   
cantus = \relative c' {
  \clef soprano
  r1 e1.  d2 d1 dis\breve e1.  gis2 gis1 a1 fis2 a2 g1 g2 f1 f2 fis2
  g2.  fis8 e8 fis2 g1 e\breve gis\breve a\breve b1 g1.  g2 fis2 fis2
  fis1 gis\breve r2 e2 fis1 g2 g2 g1 g2 g2 a1 a2 bes2 bes2.  bes4 bes2
  a2.  a4 b2 b2 e,2.  a4 d,2 f\breve f1.  e2 e1 a1 d,\breve
}

   
\score {
  \new ChoirStaff <<
    \new Staff { \new Voice { << \global \cantus >> } }
    \new Staff { \new Voice { << \global \altus >> } }
    \new Staff { \new Voice { << \global \tenor >> } }
    \new Staff { \new Voice { << \global \bassus >> } }
  >>
   \layout { }
}
