;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016,2017,2018 Ben Woodcroft <b.woodcroft@uq.edu.au>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages time)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (ice-9 regex))

;;; This package seems to work, and could be submitted to guix-devel in future.
(define-public dirseq
  (package
    (name "dirseq")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "dirseq" version))
       (sha256
        (base32
         "0vljf9v9wfjmg7z9c2ib1c6nlq0kxpzsqwqfbh0c103c282kmxhz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:modules ((srfi srfi-1)
                  (ice-9 regex)
                  (guix build ruby-build-system)
                  (guix build utils))
       #:phases
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
             (invoke "rspec" "spec/script_spec.rb")
             #t)))))
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
     ("python-dendropy" ,python2-dendropy)
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
    (version "0.0.46")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "biolib" version))
       (sha256
        (base32
         "0mmigmcwf24fz9388c360qivjnwy7amj4gv19kwmk9xhsh10sbi3"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; Python 2 only.
       #:tests? #f)) ; No tests, and imported into genomtreetk.
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)))
    (propagated-inputs
     `(("python-future" ,python2-future)))
    (home-page "http://pypi.python.org/pypi/biolib/")
    (synopsis "Library for common tasks in bioinformatics")
    (description
     "Package for common tasks in bioinformatic.") ;fixme
    (license license:gpl3)))

;; An outdated version of biopython is required for seqmagick, see
;; https://github.com/fhcrc/seqmagick/issues/59
;; When that issue has been resolved this package should be removed.
(define python2-biopython-1.66
  (package
    (inherit python2-biopython)
    (version "1.66")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "biopython" version))
              (sha256
               (base32
                "1gdv92593klimg22icf5j9by7xiq86jnwzkpz4abaa05ylkdf6hp"))))))

(define python2-seqmagick-0.6.2
  (package
    (inherit seqmagick)
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "seqmagick" version))
       (sha256
        (base32
         "0iz9jxb5idlza43h4a0h94bm4xwlhk25h5g2pbibbvgga3cn0122"))))
    (arguments
     ;; python2 only, see https://github.com/fhcrc/seqmagick/issues/56
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; Current test in setup.py does not work as of 0.6.1,
         ;; so use nose to run tests instead for now. See
         ;; https://github.com/fhcrc/seqmagick/issues/55
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (inputs
     `(("python2-biopython" ,python2-biopython-1.66)))
    (native-inputs
     `(("python-nose" ,python2-nose)))))

(define python2-biopython-1.66-instead-of-biopython
  (package-input-rewriting `((,python2-biopython . ,python2-biopython-1.66))))

(define-public graftm
  (package
    (name "graftm")
    (version "0.13.1")
    (source
     ;(local-file (string-append (getenv "HOME") "/git/graftM/dist/graftm-0.13.1.tar.gz"))
      (origin
       (method url-fetch)
       (uri (pypi-uri "graftm" version))
       (sha256
        (base32
         "174w1q8m50qywmzlf6pjanl7plw2rcs2cryhlsfb9fr10sn8lnfz")))
      )
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; ;; current test in setup.py does not work so use nose to run tests
         ;; ;; instead for now.
         (replace 'check
           (lambda _
             (setenv "PATH" (string-append (getcwd) "/bin:" (getenv "PATH")))
             ;; Some tests fail for strange reasons which seem likely to do with
             ;; being inside the chroot environment, rather than being actual
             ;; software problems.
             ;; (delete-file "test/test_external_program_suite.py")
             (zero? (system* "nosetests" "-vx"))))
         (add-after 'install 'wrap-programs-with-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (graftm (string-append out "/bin/graftM"))
                    (path (getenv "PATH")))
               (wrap-program graftm `("PATH" ":" prefix (,path))))
             #t)))))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)))
    (inputs
     `(("orfm" ,orfm)
       ("hmmer" ,hmmer)
       ("diamond" ,diamond)
       ("mfqe" ,mfqe-binary)
       ("fasttree" ,fasttree)
       ("krona-tools" ,krona-tools)
       ("pplacer" ,pplacer-binary) ; Use binary because it fails when built from source, as seen on some SingleM runs.
       ("mafft" ,mafft)
       ("perl" ,perl))) ; For interleaved files
    (propagated-inputs
     `(("taxtastic" ,python-taxtastic)
       ("python-biopython" ,python-biopython)
       ("python-biom-format" ,python-biom-format)
       ("python-extern" ,python-extern)
       ("python-h5py" ,python-h5py)
       ("python-tempdir" ,python-tempdir)
       ("python-dendropy" ,python-dendropy)))
    (home-page "http://geronimp.github.com/graftM")
    (synopsis "Identify and classify metagenomic marker gene reads")
    (description
     "GraftM is a pipeline used for identifying and classifying marker gene
reads from large metagenomic shotgun sequence datasets.  It is able to find
marker genes using hidden Markov models or sequence similarity search, and
classify these reads by placement into phylogenetic trees")
    (license license:gpl3+)))

(define-public python-extern
  (package
    (name "python-extern")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "extern" version))
       (sha256
        (base32
         "1r5ins3cpldyqrdr4kbv2aq9ymkl755h4h8nwg8z68yl5bf1mw0g"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/wwood/extern")
    (synopsis "Subprocess-related functions for ease of use")
    (description "Extern is an opinionated version of Python's subprocess, making
it more convenient to run shell commands from within Python code.  For instance,
exceptions raised by an non-zero exit status include the STDOUT and STDERR in
the description of the error.")
    (license license:expat)))

(define-public python-pytest-timeout
  (package
    (name "python-pytest-timeout")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pytest-timeout/pytest-timeout-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wq6h4w7wdpahlga8wv6zx1qj1ni4vpdycx4lq750hwb2l342ay4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page
     "http://bitbucket.org/pytest-dev/pytest-timeout/")
    (synopsis
     "py.test plugin to abort hanging tests")
    (description
     "py.test plugin to abort hanging tests")
    (license license:expat)))

(define-public python2-pytest-timeout
  (package-with-python2 python-pytest-timeout))

(define-public singlem
  (package
    (name "singlem")
    (version "0.13.0")
    (source ;(local-file (string-append (getenv "HOME") "/git/singlem/dist/singlem-" version ".tar.gz"))
     (origin
      (method url-fetch)
      (uri (pypi-uri "singlem" version))
      (sha256
       (base32
        "1n2c8bxqf68gf0fznmnfpv0vmmfsjh0byy79hzyx01cvx2d647xs")))
     )
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PATH" (string-append (getcwd) "/bin:" (getenv "PATH")))
             (invoke "bin/singlem" "--help")))
         (add-after 'install 'wrap-programs-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (graftm (string-append out "/bin/singlem"))
                    (path (getenv "PATH")))
               (wrap-program graftm `("PATH" ":" prefix (,path))))
             #t)))))
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-nose" ,python-nose)
       ("express-beta-diversity" ,express-beta-diversity)
       ("pplacer" ,pplacer-binary)))
    (inputs
     `(("vsearch" ,vsearch)
       ("krona-tools" ,krona-tools)
       ("mfqe" ,mfqe-binary)
       ("hmmer" ,hmmer)
       ("diamond" ,diamond)
       ("smafa" ,smafa-binary)
       ("graftm" ,graftm)
       ("taxtastic" ,python-taxtastic)
       ("python-extern" ,python-extern)
       ("python-tempdir" ,python-tempdir)
       ("python-dendropy" ,python-dendropy)
       ("python-biom-format" ,python-biom-format)
       ("python-h5py" ,python-h5py)
       ("python-orator" ,python-orator)
       ("python-squarify" ,python-squarify)
       ("python-matplotlib" ,python-matplotlib)))
    (home-page "http://github.com/wwood/singlem")
    (synopsis "De-novo OTUs from shotgun metagenomes")
    (description
     "SingleM is a tool to find the abundances of discrete operational taxonomic
units (OTUs) directly from shotgun metagenome data, without heavy reliance of
reference sequence databases.  It is able to differentiate closely related
species even if those species are from lineages new to science.")
    (license license:gpl3+)))

;; Cannot be contributed to the main Guix repository until pplacer has been
;; packaged.
(define-public checkm
  (package
    (name "checkm")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "checkm-genome" version))
     (sha256
      (base32
       "142i5a68kp60l26dqqajq9kzqhmpb1vz906cphfpgyhjgn0xjxg4"))))
   (build-system python-build-system)
   (arguments
    `(#:python ,python-2
      #:modules ((ice-9 popen)
                 (guix build python-build-system)
                 (guix build utils))
      #:phases
      (modify-phases %standard-phases
        (delete 'check)
        (add-after 'install 'wrap-program
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (checkm (string-append out "/bin/checkm"))
                   (path (getenv "PATH"))
                   (pythonpath (getenv "PYTHONPATH")))
              (wrap-program checkm `("PATH" ":" prefix (,path)))
              (wrap-program checkm `("PYTHONPATH" ":" prefix (,pythonpath)))
            #t)))
        (add-after 'wrap-program 'set-data-dir-and-check
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out      (assoc-ref outputs "out"))
                   (data-dir (string-append out "/share/checkm/data")))
              (setenv "PYTHONPATH" (string-append
                                    (getenv "PYTHONPATH")
                                    ":" (assoc-ref outputs "out")
                                    "/lib/python"
                                    (string-take (string-take-right
                                                  (assoc-ref inputs "python") 5) 3)
                                    "/site-packages"))

              ;; Copy data to output directory
              (copy-recursively (assoc-ref inputs "checkm-data") data-dir)
              (chmod (string-append data-dir "/.dmanifest") #o644)

              ;; Set data directory
              (let ((port (open-output-pipe "bin/checkm data setData")))
                (display data-dir port)
                (display "\n" port)
                (close-pipe port))
              (zero?
               ;; Use a simple import: the 'test' procedure uses too much RAM.
               ;; (system* (string-append (assoc-ref outputs "out") "/bin/checkm")
               ;;         "test"
               ;;          "checkm_test_results")))))))
               (system* (string-append (assoc-ref outputs "out") "/bin/checkm")
                        "-h"))))))))
   (native-inputs
    `(("python2-setuptools" ,python2-setuptools)
      ("checkm-data" ,checkm-data)))
   (inputs
    `(("hmmer" ,hmmer)
      ("prodigal" ,prodigal)
      ("pplacer" ,pplacer-binary)
      ("python2-numpy" ,python2-numpy)
      ("python2-matplotlib" ,python2-matplotlib)
      ("python2-pysam" ,python2-pysam)
      ("python2-dendropy" ,python2-dendropy)))
   (home-page "https://ecogenomics.github.io/CheckM")
   (synopsis "Assess the quality of putative genome bins")
   (description
    "CheckM provides a set of tools for assessing the quality of genomes
recovered from isolates, single cells, or metagenomes.  It provides robust
estimates of genome completeness and contamination by using collocated sets of
genes that are ubiquitous and single-copy within a phylogenetic lineage.")
   (license license:gpl3+)))

(define-public checkm-data
  (package
   (name "checkm-data")
   (version "2015_01_16")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (string-append
                  "https://data.ace.uq.edu.au/public/CheckM_databases/checkm_data_"
                  version ".tar.gz"))
            (file-name "checkm_data_2015_01_16.tar.gz")
            (sha256
             (base32
              "0b69dbw3a3wl8ck8kh86z8836i0jgxb2y54nxgcw7mlb6ilw87lp"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'build)
                     (delete 'check)
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
                                (copy-recursively "." (assoc-ref outputs "out")))))))
   (synopsis "Data for CheckM")
   (description
    "Data for CheckM")
   (home-page "https://ecogenomics.github.io/CheckM")
   (license license:gpl3+)))

;; Only used for checkm, so not contributed to main Guix repository until checkm
;; is.  Do not define-public as it is only used by CheckM, at least for the
;; moment.
(define python2-screaming-backpack
  (package-with-python2 ; python2 only
   (package
     (name "python-screaming-backpack")
     (version "0.2.333")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ScreamingBackpack" version))
        (sha256
         (base32
          "02larxvivbd2qkrlmcaynvf027ah018rwvqfwgnqkh1ywxk2phq8"))))
     (build-system python-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          ;; No tests, simply run the binary to check nothing is too bad.
          (replace 'check
            (lambda _
              (setenv "PYTHONPATH"
                      (string-append
                       (getenv "PYTHONPATH") ":."))
               (zero? (system* "bin/screamingBackpack" "-h")))))))
     (native-inputs
      `(("python2-setuptools" ,python2-setuptools)))
     (home-page
      "http://pypi.python.org/pypi/ScreamingBackpack/")
     (synopsis "ScreamingBackpack")
     (description "ScreamingBackpack")
     (license license:gpl3+))))

(define-public smafa-binary
  (package
    (name "smafa-binary")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/wwood/smafa/releases/download/v"
         version "/smafa-static-x86_64-unknown-linux-musl-"
         version ".tar.gz"))
       (sha256
        (base32
         "0ai1iqpb0g5gs49kq3028z7c51nalpw2z7v19h1gx3b9nsavisww"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check ; this is just a binary, so run rudimentary check.
           (lambda _ (zero? (system* "./smafa" "--help"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "smafa" bin))
             #t)))))
    (synopsis "Biological sequence aligner for pre-aligned sequences9")
    (description
     "Smafa attempts to align or cluster pre-aligned biological sequences,
handling sequences which are all the same length.  The main use case is through
SingleM, although it can be used without independently without issue.")
    (home-page "https://github.com/wwood/smafa")
    (license license:gpl3+)))

(define-public python-squarify
  (package
    (name "python-squarify")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "squarify" version))
       (sha256
        (base32
         "0wrg7kly2jsj42b3dr37pqdqw61rgscjffvs38hwzfsgq6wdcvlb"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/laserson/squarify")
    (synopsis
     "Pure Python implementation of the squarify treemap layout algorithm")
    (description
     "Pure Python implementation of the squarify treemap layout algorithm")
    (license #f)))

(define-public python2-squarify
  (package-with-python2 python-squarify))

(define-public enrichm
  (let ((commit "26970bbe80d9e6e3e3acd3fdb6800b60f7d28cfe"))
    (package
     (name "enrichm")
     (version (string-append "0.2.0-2." (string-take commit 8)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/geronimp/enrichM.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1hv5vcsrcxha81mnzqpq3azw00wgb2r5nmyw9kxvfabjwxsja9ss"))
              (patches (search-patches "enrichm-path1.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (path (getenv "PATH"))
                    (db (assoc-ref inputs "enrichm-data")))
               (wrap-program (string-append bin "/enrichm")
                             `("PATH" ":" prefix (,path)))
               (wrap-program (string-append bin "/enrichm")
                             `("ENRICHM_DB" ":" = (,db))))
             )))
       ))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (inputs
     `(("enrichm-data" ,enrichm-data)
       ("python-dateutil+" ,python-dateutil)
       ("python-statsmodels" ,python-statsmodels)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-scipy" ,python-scipy)
       ("python-biopython" ,python-biopython)
       ("python-six" ,python-six)
       ("python-sklearn" ,python-scikit-learn)
       ("hmmer" ,hmmer)
       ("seqmagick" ,seqmagick)
       ("diamond" ,diamond)
       ("prodigal" ,prodigal)
       ("parallel" ,parallel)
       ("mmseqs" ,mmseqs)
       ("r" ,r)))
    (home-page "")
    (synopsis "toolbox to compare functional composition of population genomes")
    (description
     "EnrichM is a toolbox for comparing the functional composition of
population genomes.")
    (license license:gpl3+))))

(define-public enrichm-data
  (package
   (name "enrichm-data")
   (version "8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://data.ace.uq.edu.au/public/enrichm/enrichm_database_v"
                  version ".tar.gz"))
            (sha256
             (base32
              "0h8jzj020gjjfyc3hgg9pmaksa3v740rsh6pa1nbjnzz9rvpzbqz"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'build)
                     (delete 'check)
                     (replace 'install
                              (lambda* (#:key outputs #:allow-other-keys)
				       (let ((out (assoc-ref outputs "out")))
					 (symlink out "26-11-2018")
					 (copy-recursively "." out)
					 #t))))))
   (synopsis "Data for EnrichM")
   (description
    "Data for EnrichM")
   (home-page "https://github.com/geronimp/enrichM")
   (license license:gpl3+)))

(define-public refinem
  (package
    (name "refinem")
    (version "0.0.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "refinem" version))
       (sha256
        (base32
         "14k3drncdy559v3ibr44scxds1jyk5dv7w27mh4bv3z1m9fx2q13"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (inputs
     `(("python-biolib" ,python2-biolib)
       ("python-jinja2" ,python2-jinja2)
       ("python-matplotlib" ,python2-matplotlib)
       ("python-mpld3" ,python2-mpld3)
       ("python-numpy" ,python2-numpy)
       ("python-pysam" ,python2-pysam)
       ("python-weightedstats" ,python2-weightedstats)
       ("python-dendropy" ,python2-dendropy)
       ("python-scipy" ,python2-scipy)
       ("prodigal" ,prodigal)
       ("blast+" ,blast+)
       ("diamond" ,diamond)
       ("krona-tools" ,krona-tools)))
    (home-page
     "http://pypi.python.org/pypi/refinem/")
    (synopsis
     "A toolbox for improving population genomes.")
    (description
     "A toolbox for improving population genomes.")
    (license license:gpl3)))

(define-public mfqe-binary
  (package
    (name "mfqe-binary")
    (version "0.5.0")
    (source
     ;; (local-file (string-append (getenv "HOME") "/git/coverm/target/release")
     ;;             #:recursive? #t))
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wwood/mfqe/releases/download/v"
                           version
                           "/mfqe-static-x86_64-unknown-linux-musl-"
                           version
                           ".tar.gz"))
       (sha256
        (base32
         "1x9ja9xg0grmcjd8dl1vyy8pg1hgy11q2zhqdpnwmi32y171608z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'check
                  (lambda _
                    (invoke "./mfqe" "-h")))
         ;; ;; (delete 'strip) ; Does not work. Eh.
         ;; (delete 'validate-runpath)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin/"))
                    (file "mfqe"))
               (install-file file bin)
               #t))))))
    (synopsis "Extract one or more sets of reads by name from a FASTA/Q file")
    (description
     "Extract one or more sets of reads from a FASTQ (or FASTA) file by
specifying their read names.")
    (home-page "https://github.com/wwood/mfqe")
    (license license:gpl3+)))
