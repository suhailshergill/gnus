#! /bin/sh
#|
exec mzscheme -mr $0 ${1+"$@"}
|#
(require (lib "cmdline.ss"))
(define +scmfile+ "xml2texi.scm")
(define +in+ "gnus-faq.xml")
(define +out+ "gnus-faq.texi")

(command-line
 "xml2texi"
 (current-command-line-arguments)
 (once-each
  (("-i" "--input") in "Name of XML data file (default gnus-faq.xml)"
   (set! +in+ in))
  (("-o" "--output") out "Name of output file (default gnus-faq.texi)"
   (set! +out+ out))
  (("-l" "--library") lib "Name of Scheme library to load (default faqxml2texi.scm)"
   (set! +scmfile+ lib)))
 (help-labels "The first (or only) remaining argument is used as the name of the Input file"
              "The second argument is used as the name of the Output file")
 (args infile+outfile
       (cond ((= (length infile+outfile) 2)
               (set! +in+ (car infile+outfile))
               (set! +out+ (cadr infile+outfile)))
             ((= (length infile+outfile) 1)
               (set! +in+ (car infile+outfile)))
             (else #f))))

(load +scmfile+)
(main +in+ +out+)
