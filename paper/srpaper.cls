%%
%% This is file `srpaper.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% srpaper.dtx  (with options: `class')
%% 
%%  Rett Bull (Everett.Bull@pomona.edu)
%%  Pomona College, Computer Science Department
%% 
%%  License:
%% 
%%    This program is free software; you can redistribute it and/or
%%    modify it under the terms of the GNU General Public License as
%%    published by the Free Software Foundation; either version 2 of the
%%    License, or (at your option) any later version.
%% 
%%    This program is distributed in the hope that it will be useful,
%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%    GNU General Public License for more details.
%% 
%%    You should have received a copy of the GNU General Public License
%%    along with this program; if not, write to the Free Software
%%    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%    02111-1307  USA
%% 
%%  The text for the copyright paragraph is taken from hmcthesis.cls,
%%  by Claire Connolly and Mellisa O'Neill. That work is protected
%%  by the same license as appears above.
%% 
\def\fileversion{1.01}
\def\filedate{June 8, 2007}
\def\docdate{August 8, 2007}
\def\filename{srpaper.cls}
\def\classname{srpaper}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{srpaper}
  [2007/06/08 v1.01
   LaTeX class for Pomona College Computer Science Senior Paper]
\LoadClass[11pt,oneside,openright,onecolumn]{report}
\newcommand{\local@classerror}[1]{\ClassError\classname{#1}\@ehc}
\DeclareOption{finalcopy}{\@draftfalse
                          \let\@drafttrue\incompat@mode}
\DeclareOption{draftcopy}{\@drafttrue
                          \let\@draftfalse\incompat@mode}
\DeclareOption{short}{\@shorttrue}
\DeclareOption{singlespace}{\@select@spacing{singlespacing}}
\DeclareOption{onehalfspace}{\@select@spacing{onehalfspacing}}
\DeclareOption{doublespace}{\@select@spacing{doublespacing}}
\DeclareOption{lof}{\@loftrue
                    \let\@loffalse\incompat@lof}
\DeclareOption{nolof}{\@loffalse
                      \let\@lofrue\incompat@lof}
\DeclareOption{lot}{\@lottrue
                    \let\@lotfalse\incompat@lot}
\DeclareOption{nolot}{\@lotfalse
                      \let\@lotrue\incompat@lot}
\DeclareOption{cm}{\@select@font{}}
\DeclareOption{mathtime}{\@select@font{mathtime}}
\newif\if@draft        \@draftfalse
\newif\if@short        \@shortfalse
\newif\if@lof          \@loftrue
\newif\if@lot          \@lottrue
\def\@selectedspacing{}
\def\@select@spacing#1{
  \gdef\@selectedspacing{#1}
  \global\let\@select@spacing\incompat@spacing}
\def\@selectedfont{}
\def\@select@font#1{
  \gdef\@selectedfont{#1}
  \global\let\@select@font\incompat@font}
\DeclareOption*{\local@classerror{Unknown option \CurrentOption}}
\def\incompatible@options#1{\local@classerror{Incompatible options: #1}}
\def\incompat@mode{\incompatible@options{finalcopy and draftcopy}}
\def\incompat@spacing{\incompatible@options{singlespace, %
  onehalfspace, and doublespace}}
\def\incompat@lof{\incompatible@options{lof and nolof}}
\def\incompat@lot{\incompatible@options{lot and nolot}}
\def\incompat@font{\incompatible@options{cm and mathtime}}
\ProcessOptions
\ifx \@selectedfont\@empty \relax
\else
  \RequirePackage{\@selectedfont}
\fi
\ifx \@selectedspacing\@empty
  \if@draft
    \def\@selectedspacing{doublespacing}
  \else
    \def\@selectedspacing{singlespacing}
  \fi
\fi
\newcommand{\@if@finalns}[1]{\if@draft\else\if@short\else#1\fi\fi}
\RequirePackage{alltt}
\RequirePackage{ifthen}
\RequirePackage{verbatim}
\def\abstractname{Abstract}
\def\acknowledgmentname{Acknowledgments}
\def\prefacename{Preface}
\def\institutionname{Pomona College}
\def\subjectname{Computer Science}
\def\departmentname{Department of \subjectname}
\def\submissiontext{%
  Submitted as part of the senior exercise for the degree of\\
  Bachelor of Arts in \subjectname}
\def\copyrighttext{%
  The author grants Pomona College the nonexclusive right to make
  this work available for noncommercial, educational purposes,
  provided that this copyright statement appears on the reproduced
  materials and notice is given that the copying is by permission of
  the author.  To disseminate otherwise or to republish requires
  written permission from the author.}
\newcommand{\advisor}[1]{\gdef\@advisor{#1}}
\long\def\abstract#1{\gdef\@abstract{#1}}
\long\def\acknowledgment#1{\gdef\@acknowledgment{#1}}
\gdef\@advisor{}
\gdef\@abstract{}
\gdef\@acknowledgment{}
\gdef\@title{}
\gdef\@author{}
\gdef\@date{}
\def\frontmatter@check{%
  \ifx \@title\@empty \local@classerror{No title specified} \fi
  \ifx \@author\@empty \local@classerror{No author specified} \fi
  \ifx \@date\@empty \local@classerror{No date specified} \fi
  \@if@finalns{%
    \ifx \@advisor\@empty \local@classerror{No advisor specified} \fi
    \ifx \@abstract\@empty \local@classerror{No abstract specified} \fi}}
\renewcommand{\maketitle}{%
  \begin{titlepage}%
  \begin{center}
  \institutionname\\
  \departmentname
  \end{center}
  \vfill
  \begin{center}
  {\LARGE\@title}\\[3em]
  {\large\@author}\\[1.5em]
  {\large\@date}
  \end{center}
  \vfill\vfill
  \begin{center}
  \submissiontext\\[0.5em]
  \@advisor
  \end{center}
  \end{titlepage}%
  \setcounter{footnote}{0}}
\def\extract@year#1, #2END{#2}
\newcommand{\@copyrightpage}{%
  \strut\vfill
  \begin{center}\small
  \begin{minipage}{0.8\normal@textwidth}
  \setlength{\parskip}{0.5\normalbaselineskip}
  Copyright \copyright~\expandafter\extract@year\@date END \@author
  \par
  \copyrighttext
  \end{minipage}
  \end{center}
  \clearpage}
\newcommand{\@frontsection}[2]{%
  \cleardoublepage
  \vspace*{50pt}
  \addcontentsline{toc}{section}{#1}
  \begin{center}
  \begin{minipage}{0.8\normal@textwidth}
  \setlength{\parskip}{0.5\normalbaselineskip}
  \section*{#1}#2
  \end{minipage}
  \end{center}}
\newcommand{\@frontlist}[2]{%
  \cleardoublepage
  \chapter*{#2}%
  \ifthenelse{\equal\contentsname{#2}}
    {}
    {\addcontentsline{toc}{section}{#2}}
  \@starttoc{#1}}
\newcommand{\frontmatter}{%
  \frontmatter@check
  \pagestyle{empty}
  \maketitle
  \clearpage
  \@copyrightpage
  \pagenumbering{roman}
  \pagestyle{plain}
  \@frontsection{\abstractname}{\@abstract}
  \ifx \@acknowledgment\@empty
  \else
    \@frontsection{\acknowledgmentname}{\@acknowledgment}
  \fi
  \@frontlist{toc}{\contentsname}
  \if@lof
    \@frontlist{lof}{\listfigurename}
  \fi
  \if@lot
    \@frontlist{lot}{\listtablename}
  \fi
  \cleardoublepage
  \def\@chapter@one@hook{\pagenumbering{arabic}}
  \@clean@up
  \restorespacing
  \@narrowmargins}
\if@short
  \renewcommand{\frontmatter}{%
    \frontmatter@check
    \@narrowmargins
    \vskip 2em
    \begin{center}
    {\LARGE\@title}\\[1.5em]
    {\large\@author}\\[1em]
    {\large\@date}
    \end{center}
    \par\vskip 1.5em
    \pagestyle{plain}
    \thispagestyle{empty}
    \pagenumbering{arabic}
    \ifx \@abstract\@empty \else
      \begin{center}
      \textbf{\abstractname}\\[0.5\normalbaselineskip]
      \begin{minipage}{0.8\normal@textwidth}
      \setlength{\parskip}{0.5\normalbaselineskip}
      \small \@abstract
      \end{minipage}
      \end{center}
      \vskip 1.5em
    \fi
    \@clean@up
    \restorespacing}
\fi
\newcommand{\@clean@up}{%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  \global\let\@advisor\@empty
  \global\let\@abstract\@empty
  \global\let\@acknowledgment\@empty
  \global\let\@copyrighttext\@empty
  \global\let\@submissiontext\@empty}
\DeclareRobustCommand\bfseries
        {\not@math@alphabet\bfseries\mathbf
         \fontseries\bfdefault\selectfont
         \boldmath}
\if@draft
  \newcount\@hours
  \newcount\@minutes
  \@tempcnta \time
  \@hours\@tempcnta
  \divide \@hours 60
  \@minutes -\@hours
  \multiply \@minutes 60
  \advance \@minutes \@tempcnta
  \ifnum 0=\@hours
     \@hours 12
  \else \ifnum 12 < \@hours
     \advance \@hours -12
  \fi\fi
  \edef\drafttime{\number\@hours:\two@digits{\@minutes}
                 \ifnum \@tempcnta < 720 am\else pm\fi}
  \def\draftheader{Draft of \today, at \drafttime}
  \let\ps@empty@saved\ps@empty
  \def\ps@empty{%
    \ps@empty@saved
    \def\@oddhead{\hss\textsf{\draftheader}\hss}
    \let\@evenhead\@oddhead}
  \let\ps@plain@saved\ps@plain
  \def\ps@plain{%
    \ps@plain@saved
    \def\@oddhead{\hss\textsf{\draftheader}\hss}
    \let\@evenhead\@oddhead}
\fi
\newlength\normal@textwidth
\setlength{\normal@textwidth}{\textwidth}
\ifthenelse{\equal\@selectedspacing{singlespacing}}
  {\let\@normalmargins\relax
   \let\@narrowmargins\relax}
  {\newlength\normal@margin
   \setlength{\normal@margin}{\oddsidemargin}
   \newcommand{\@adjustmargins}[1]{%
     \clearpage%
     \setlength{\@tempdima}{#1}%
     \@settopoint\@tempdima%
     \setlength{\oddsidemargin}{\normal@margin}%
     \addtolength{\oddsidemargin}{1\@tempdima}%
     \setlength{\evensidemargin}{\oddsidemargin}%
     \setlength{\textwidth}{\normal@textwidth}%
     \addtolength{\textwidth}{-2\@tempdima}%
     \setlength{\linewidth}{\textwidth}%
     \setlength{\columnwidth}{\textwidth}%
     \setlength{\hsize}{\textwidth}}%
   \newcommand{\@normalmargins}{\@adjustmargins{0pt}}%
   \newcommand{\@narrowmargins}{\@adjustmargins{-0.5in}}}
\ifthenelse{\equal\@selectedspacing{singlespacing}}
  {\let\restorespacing\relax
   \let\displayspacing\relax
   \let\@singlespacing\relax}
  {\RequirePackage[singlespacing]{setspace}
   \ifthenelse
     {\equal\@selectedspacing{onehalfspacing}}
     {\let\restorespacing\onehalfspacing}
     {\let\restorespacing\doublespacing}
   \newcommand{\@singlespacing}{\setstretch{\setspace@singlespace}}
   \newcommand{\displayspacing}{%
     \vskip\baselineskip%
     \@singlespacing%
     \vskip-\baselineskip}}
\newcommand{\preface}{%
  \chapter*{\prefacename}
  \addcontentsline{toc}{chapter}{\prefacename}}
\renewcommand{\part}{\local@classerror{part is not used in srpaper.}}
\if@short
  \renewcommand{\chapter}{\@startsection{chapter}{0}{\z@}%
                                        {-5ex \@plus -1ex \@minus -.2ex}%
                                        {3ex \@plus.2ex}%
                                        {\normalfont\LARGE\bfseries}}
\else
  \let\@chapter@saved\@chapter
  \let\@chapter@one@hook\relax
  \def\@chapter[#1]#2{%
    \@chapter@one@hook
    \@chapter@saved[#1]{#2}
    \global\let\@chapter\@chapter@saved}
\fi
\let\saved@makechapterhead\@makechapterhead
\renewcommand{\@makechapterhead}[1]{%
  \begingroup
  \@singlespacing
  \saved@makechapterhead{#1}
  \endgroup}
\let\saved@makeschapterhead\@makeschapterhead
\renewcommand{\@makeschapterhead}[1]{%
  \begingroup
  \@singlespacing
  \saved@makeschapterhead{#1}
  \endgroup}
\let\saved@startsection\@startsection
\renewcommand{\@startsection}[6]{%
  \saved@startsection{#1}{#2}{#3}{#4}{#5}{\@singlespacing#6}}
\renewcommand{\cleardoublepage}{%
  \clearpage
  \@if@finalns{\ifodd\c@page\else\strut\newpage\fi}}
\setcounter{tocdepth}{1}
\def\thebibstyle{alpha}
\let\biblio@saved\bibliography
\renewcommand{\bibliography}[1]{%
  \bibliographystyle{\thebibstyle}
  \begingroup
  \displayspacing
  \let\bibchapter@saved\chapter
  \def\chapter##1##2{%
    \bibchapter@saved##1{##2}%
    \addcontentsline{toc}{chapter}{##2}}
  \biblio@saved{#1}
  \clearpage
  \endgroup}
\def\theannbibstyle{annotate}
\newcount\clubpenalty@saved
\newcount\widowpenalty@saved
\newcount\sfperiod@saved
\newcommand{\annotatedbibliography}[1]{%
  \bibliographystyle{\theannbibstyle}
  \begingroup
  \let\@listi@saved\@listi
  \def\@listi{\@listi@saved%
              \addtolength{\itemsep}{0.5\topsep}%
              \addtolength{\topsep}{0.5\topsep}}
  \clubpenalty@saved \clubpenalty
  \widowpenalty@saved \widowpenalty
  \sfperiod@saved \sfcode`\.
  \renewenvironment{quotation}{\par
                               \clubpenalty \clubpenalty@saved
                               \widowpenalty \widowpenalty@saved
                               \sfcode`\. \sfperiod@saved}{}
  \def\chapter##1##2{}
  \biblio@saved{#1}
  \endgroup}
\newenvironment{indented}
  {\list{}{}\item}
  {\endlist}
\newenvironment{indented*}
  {\list{}{}\item\displayspacing}
  {\endlist}
\newenvironment{code}
  {\begin{indented*}\begin{alltt}}
  {\end{alltt}\end{indented*}}
\newenvironment{vcode}
  {\csname indented*\endcsname\verbatim}
  {\endverbatim\csname endindented*\endcsname}
\InputIfFileExists{srpaper.cfg}{}{}



\endinput
%%
%% End of file `srpaper.cls'.
