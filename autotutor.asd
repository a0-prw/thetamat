(in-package :asdf)

(defsystem "autotutor"

    :description "Thetamat is 'The Teaching Assistant for
Mathematics'.  It is a system of interactive lessons and exercises for
teaching and demonstrating elementary mathematics.  The lessons and
exercises are made available through a web-browser interface for use
both in class teaching situations by subscribed teachers and in
individual practice and lesson-review by their enrolled pupils. 

The system guides pupils through the exercises, generating immediate
feedback for the pupil and progress reports which can be accessed by
the teacher.  The lessons provide both traditional text (containing
explicit instruction) and appropriate interactive elements which are
designed to support the understanding of the textual content.

Thetamat can supplement (and in some cases, depending on how the
teacher chooses to use it, replace) a traditional school textbook.
The program is not intended as a self-tutoring system, though more
mature, motivated students with good reading ability may be able to
use it in this way.

Thetamat may be particularly useful in teaching groups of pupils of
widely diverging levels of mathematical understanding, since it allows
pupils to progress at their own speed, while helping the teacher to
manage and monitor the pupils interaction with the teaching material.

The system is not intended to teach problem solving skills but aims to
assist in the teaching of basic skills and knowledge which are
prerequisite to developing mathematical problem-solving ability."

    :version "0.1.4"
    :author "Peter Wood, pete_wood@runbox.com"
    :license " GPL3"
    :depends-on (:hunchentoot  
                 :flexi-streams 
                 :ironclad	 
                 :parenscript  
                 :st-json 
                 :cl-who  
                 :cl+ssl 
                 :simple-date 
                 :cl-store 
                 :split-sequence)
    :serial t
    :components (
                 (:module "Infrastructure"
                          :serial t
                          :components ((:file "package") 
                                       (:file "macro-utils")
                                       (:module "Translations" 
                                                :serial t 
                                                :components ((:file "translate")))
                                       (:file "pathnames")))
                 (:module "Framework"
                          :serial t
                          :components ((:file "ajax")
                                       (:file "safe-access-macros" ) 
                                       (:file "conditions" )
                                       (:file "delimiter" ) 
                                       (:file "type-specs" ) 
                                       (:file "decimal" ) 
                                       (:file "util" )  
                                       (:file "standalone")
                                       (:file "user" )
                                       (:file "record")
                                       (:file "pupil-report")
                                       (:file "groups" )))
                 (:module "Content" 
                          :serial t 
                          :components ((:file "learning-space") 
                                       (:file "modules")
                                       (:file "didactic-elements")
                                       (:file "define-lesson")))
                 (:module "Production"
                          :serial t
                          :components ((:file "g1") 
                                       (:file "latex" ) 
                                       (:file "configure-document" )  
                                       (:file "document" )
                                       (:file "layout" )
                                       ;;
                                       (:file "page-utils" )
                                       ))
		 (:module "Scripts" 
                          :serial t 
                          :components 
                          ((:file "dom-utils") 
                           (:file "scratchpad")
                           (:file "common" ) 
                           (:file "pupil" ) 
                           (:file "login" ) 
                           (:file "teacher")
                           (:file "html")
                           (:module "Lessons" 
                                    :serial t 
                                    :components 
                                    ((:file "lesson-modules-ne")
                                     (:file "generator-links")
                                     (:file "mod-digs-nat")
                                     (:file "mod-basic-add-nat")
                                     (:file "mod-basic-sub-nat")
                                     (:file "mod-position-sys1-nat")
                                     (:file "mod-sub-position-sys-nat")
                                     (:file "mod-position-sys2-nat")
                                     (:file "mod-basic-int")
                                     (:file "mod-table")
                                     (:file "mod1-mult")
                                     (:file "mod2-int")
                                     (:file "mod2-mult")
                                     (:file "mod1-div-int")
                                     (:file "mod2-div-int")
                                     ))))
                 (:module "Net" 
                          :serial t 
                          :components 
                          ((:file "tbnl-config")
                           ))
                 (:file "if")
                 )
    )
