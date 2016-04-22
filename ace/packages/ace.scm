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



(define-module (ace packages ace)
  #:use-module (ace packages external)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby))
  
;;; This package seems to work, and could be submitted to guix-devel in future.
(define-public dirseq
  (package
    (name "dirseq")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "dirseq" version))
       (sha256
        (base32
         "1fixvy3zapl16x71nlsra2g1c3lgf220rmqs5d0llpcd0k4b7hjf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-paths-to-inputs
           (lambda _
             (substitute* "bin/dirseq"
               (("\\\"sed") (string-append "\"" (which "sed")))
               (("\\\"samtools") (string-append "\"" (which "samtools")))
               (("\\\"bedtools") (string-append "\"" (which "bedtools"))))
             #t))
         ;; Call rspec directly so jeweler is not required.
         (replace 'check
           (lambda _
             (zero? (system* "rspec" "spec/script_spec.rb")))))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec-2)))
    (inputs
     `(("bedtools" ,bedtools)
       ("samtools" ,samtools)))
    (propagated-inputs
     `(("bioruby" ,bioruby)
       ("ruby-bio-commandeer" ,ruby-bio-commandeer)
       ("ruby-bio-logger" ,ruby-bio-logger)))
    (synopsis "Gene expression calculator for metatranscriptomics")
    (description
     "Dirseq is a calculates gene expression metrics, aimed particularly at
metatranscriptomic data mapped to population genomes.  DirSeq works out whether
RNAseq reads from metatranscriptomes are generally in the same direction as the
ORF predicted and provide gene-wise coverages using DNAseq mappings.")
    (home-page "http://github.com/wwood/dirseq")
    (license license:expat)))

(define-public genometreetk
  (package
  (name "genometreetk")
  (version "0.0.21")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "GenomeTreeTk" version))
      (sha256
        (base32
          "0yb4q9vhybgcl83c6r5x7fbaf2sr2xpxwp9vkwd2iqbi57ki3l5v"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2 ; Python 2 only.
     #:phases
     (modify-phases %standard-phases
       (replace 'check
         (lambda _
           ;; There are no tests, do a simple import test.
           (setenv "PYTHONPATH" (string-append (getenv "PYTHONPATH") ":."))
           (zero? (system* "bin/genometreetk" "-h"))))
       (add-after 'install 'wrap-binary
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (bin  (string-append out "/bin/"))
                   (path (getenv "PATH")))
              (wrap-program (string-append bin "/genometreetk")
                `("PATH" ":" prefix
                  (,path))))
            #t)))))
  (inputs
   `(("python-setuptools" ,python2-setuptools)
     ("python-numpy" ,python2-numpy)
     ("python-dendropy" ,python2-dendropy-untested)
     ("python-biolib" ,python2-biolib)
     ("fasttree" ,fasttree)
     ("hmmer" ,hmmer)))
  (home-page
    "http://pypi.python.org/pypi/genometreetk/")
  (synopsis "A toolbox for working with genome trees.")
  (description
    "A toolbox for working with genome trees.")
  (license license:gpl3)))

(define-public python2-biolib
  (package
    (name "python2-biolib")
    (version "0.0.26")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "biolib" version))
       (sha256
        (base32
         "0crs8134fycb2mr5d70jjxhk889cmwx2vch81qp28vf6jwkciwy2"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; Python 2 only.
       #:tests? #f)) ; No tests, and imported into genomtreetk.
    (inputs
     `(("python-setuptools" ,python2-setuptools)))
    (home-page "http://pypi.python.org/pypi/biolib/")
    (synopsis "Library for common tasks in bioinformatics")
    (description
     "Package for common tasks in bioinformatic.") ;fixme
    (license license:gpl3)))

(define-public python2-dendropy-untested
  (package
    (inherit python2-dendropy)
    (arguments
     `(#:python ,python-2
        #:tests? #f))))

;; Cannot be contributed to the main Guix repository until pplacer has been
;; packaged.
(define-public checkm
  (package
   (name "checkm")
   (version "1.0.5")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "checkm-genome" version))
     (sha256
      (base32
       "13cm0401y4wvhyx0bxpfac0y3nkyx0y1b2w07mmvsiiw1dbqv870"))))
   (build-system python-build-system)
   (arguments
    `(#:python ,python-2
      #:phases
      (modify-phases %standard-phases
        (replace 'check
                 (zero? (system* "bin/checkm" "test" "checkm_test_results"))))))
   (native-inputs
    `(("python2-setuptools" ,python2-setuptools)))
   (propagated-inputs
    `(("hmmer" ,hmmer)
      ("prodigal" ,prodigal)
      ("pplacer" ,pplacer)))
   (home-page "https://ecogenomics.github.io/CheckM")
   (synopsis "Assess the quality of putative genome bins")
   (description
    "CheckM provides a set of tools for assessing the quality of genomes
recovered from isolates, single cells, or metagenomes.  It provides robust
estimates of genome completeness and contamination by using collocated sets of
genes that are ubiquitous and single-copy within a phylogenetic lineage.")
   (license license:gpl3+)))
