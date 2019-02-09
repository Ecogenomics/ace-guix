;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ben Woodcroft <b.woodcroft@uq.edu.au>
;;;
;;; This file is part of the Australian Centre for Ecogenomics' GNU Guix package
;;; repository.
;;;
;;; The repository is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; The repository is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; the repository.  If not, see <http://www.gnu.org/licenses/>.


(define-module (ace packages external)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages vim))

;;; The pplacer that is installed with Guix on occasion generates errors like so:
;;; Uncaught exception: Multiprocessing.Child_error(_)
;;;
;;; OTOH, the binaries compiled do not. So, they have a use.
;;;
;;; See also
;;; https://github.com/matsen/pplacer/issues/354
(define-public pplacer-binary
  (package
    (name "pplacer-binary")
    (version "1.1.alpha19")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/matsen/pplacer/releases/download/v"
                       version
                       "/pplacer-Linux-v"
                       version
                       ".zip"))
       (sha256
        (base32
         "0z9ljalh68y2912wsd22fbzdzxaaysf18prqcvw2gffx6mfb8cci"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check ; this is just a binary, so run rudimentary check.
           (lambda _ (zero? (system* "./pplacer" "--help"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "pplacer" bin)
               (install-file "guppy" bin)
               (install-file "rppr" bin))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (synopsis "Place query sequences on a fixed reference phylogenetic tree")
    (description
     "Pplacer places query sequences on a fixed reference phylogenetic tree to
maximize phylogenetic likelihood or posterior probability according to a
reference alignment.  Pplacer is designed to be fast, to give useful information
about uncertainty, and to offer advanced visualization and downstream
analysis.")
    (home-page "http://matsen.fhcrc.org/pplacer")
    (license license:gpl3)))


(define-public mmseqs ; Uses -march=native but probably works. For tests need
                      ; updated googletest in guix proper I'd say. Bundles a few
                      ; libraries
  (package
   (name "mmseqs")
   (version "2-23394")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/soedinglab/MMseqs2/archive/"
                                version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "0wkhnrkhya0l2ayib50c1s24nbxxx4p8ll1pzinapqad9kzkyxin"))))
   (build-system cmake-build-system)
   (arguments
    ;; There are no tests, see https://github.com/soedinglab/mmseqs2/issues/25
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'delete-bundled-code
                                (lambda _
                                  ;; Delete bundled gzstream. Other entries in the lib/
                                  ;; directory appear not to have any primary sources, or in the
                                  ;; case of kseq, appears to be either out of date or has been
                                  ;; modified relative to its original form.
                                        ;(delete-file "lib/kseq/kseq.h")
                                  ;; (delete-file-recursively "lib/gzstream")
                                  ;; (substitute* '("src/commons/A3MReader.cpp"
                                  ;;                "src/commons/Util.cpp"
                                  ;;                "src/util/createdb.cpp"
                                  ;;                "src/util/extractdomains.cpp"
                                  ;;                "src/test/TestDiagonalScoringPerformance.cpp"
                                  ;;                "src/test/TestAlignmentPerformance.cpp")
                                  ;;   (("^#include \\\"kseq.h\\\"\n$")
                                  ;;    "#include \"htslib/kseq.h\"\n"))
                                  #t)))))
   (inputs
    `(("htslib" ,htslib)
      ("gzstream" ,gzstream)))
   (native-inputs
    `(("xxd" ,vim)
      ("googletest" ,googletest)))
   (home-page "http://mmseqs.com")
   (synopsis "Fast and sensitive protein search and clustering")
   (description
    "MMseqs2 (Many-against-Many searching) is a software suite to search and
cluster huge protein sequence sets.  The software is designed to run on
multiple cores and servers and exhibits very good scalability.  MMseqs2 can run
10000 times faster than BLAST.  At 100 times its speed it achieves the same
sensitivity.  It can also perform profile searches with the same sensitivity as
PSI-BLAST but at around 270 times its speed.")
   (license license:gpl3+))) ; need to check actual code

(define-public python-mpld3
  (package
    (name "python-mpld3")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpld3" version))
       (sha256
        (base32
         "1dwahw0y6rs9my6pvr2xjnjvcysw8dchfxncgsrrkgqila25hiad"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Tests fail because there is no display
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-jinja2" ,python-jinja2)))
    (home-page "http://mpld3.github.io")
    (synopsis "D3 Viewer for Matplotlib")
    (description "D3 Viewer for Matplotlib")
    (license #f)))

(define-public python2-mpld3
  (package-with-python2 python-mpld3))

(define-public python-weightedstats
  (package
    (name "python-weightedstats")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "weightedstats" version))
       (sha256
        (base32
         "064gyjiixllf5vlq9ik8zk83kjcvswxhxj8p0i9h4ar48j78bzf5"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/tinybike/weightedstats")
    (synopsis
     "Mean, weighted mean, median, weighted median")
    (description
     "Mean, weighted mean, median, weighted median")
    (license license:expat)))

(define-public python2-weightedstats
  (package-with-python2 python-weightedstats))
