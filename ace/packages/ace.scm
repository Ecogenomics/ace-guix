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
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web))

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

(define-public graftm
  (let ((commit "a64a2b7ed83e98546ac2c4e7b3218245ef84f852"))
    (package
      (name "graftm")
      (version (string-append "0.9.5-2." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/geronimp/graftM.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0y2b90fh42xdjim29jzja611828yml0vnwv9h181wsdvw8yy82hh"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2 ; python-2 only
         #:phases
         (modify-phases %standard-phases
           ;; current test in setup.py does not work so use nose to run tests
           ;; instead for now.
           (replace 'check
             (lambda _
               ;(setenv "TEMPDIR" "/tmp") ; not sure if this is needed. 
               (setenv "PATH" (string-append (getcwd) "/bin:" (getenv "PATH")))
               ;(zero? (system* "python" "test/test_graft.py"))))
               ;; Some tests fail for strange reasons which seem likely to do with
               ;; being inside the chroot environment, rather than being actual
               ;; software problems.
               (delete-file "test/test_archive.py")
               (delete-file "test/test_external_program_suite.py")
               (zero? (system* "nosetests" "-vx"))))
           (add-after 'install 'wrap-programs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (graftm (string-append out "/bin/graftM"))
                      (path (getenv "PATH")))
                 (wrap-program graftm `("PATH" ":" prefix (,path))))
               #t)))))
      (native-inputs
       `(("python-setuptools" ,python2-setuptools)
         ("python-nose" ,python2-nose)))
      (inputs
       `(("python-biopython" ,python2-biopython)
         ("python-subprocess32" ,python2-subprocess32)
         ("python-biom-format" ,python2-biom-format)
         ("python-extern" ,python2-extern)
         ("python-h5py" ,python2-h5py)
         ("python-tempdir" ,python2-tempdir)
         ("python-dendropy" ,python2-dendropy)
         ("orfm" ,orfm)
         ("hmmer" ,hmmer)
         ("diamond" ,diamond-0.7.9) ; Test data is made with an old diamond version.
         ("fxtract" ,fxtract)
         ("fasttree" ,fasttree)
         ("krona-tools" ,krona-tools)
         ("pplacer" ,pplacer)
         ("seqmagick" ,seqmagick)
         ("taxtastic" ,taxtastic)
         ("mafft" ,mafft)))
      (home-page "http://geronimp.github.com/graftM")
      (synopsis "Identify and classify metagenomic marker gene reads")
      (description
       "GraftM is a pipeline used for identifying and classifying marker gene
reads from large metagenomic shotgun sequence datasets.  It is able to find
marker genes using hidden Markov models or sequence similarity search, and
classify these reads by placement into phylogenetic trees")
      (license license:gpl3+))))

(define-public diamond-0.7.9
  (package
    (inherit diamond)
    (name "diamond")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/bbuchfink/diamond/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hfkcfv9f76h5brbyw9fyvmc0l9cmbsxrcdqk0fa9xv82zj47p15"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f  ;no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-dir
                    (lambda _
                      (chdir "src")
                      #t))
         (delete 'configure)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin")))
                      (mkdir-p bin)
                      (copy-file "../bin/diamond"
                                 (string-append bin "/diamond"))
                      #t))))))
    (native-inputs
     `(("bc" ,bc)))
    (inputs
     `(("boost" ,boost)
       ("zlib" ,zlib)))))

(define-public python-tempdir
  (package
    (name "python-tempdir")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tempdir" version))
       (sha256
        (base32
         "13msyyxqbicr111a294x7fsqbkl6a31fyrqflx3q7k547gnq15k8"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page
     "https://bitbucket.org/another_thomas/tempdir")
    (synopsis
     "Tempdirs are temporary directories, based on tempfile.mkdtemp")
    (description
     "Tempdirs are temporary directories, based on tempfile.mkdtemp")
    (license expat)
    (properties `((python2-variant . ,(delay python2-pytest-cache))))))

(define-public python2-tempdir
  (package-with-python2 (strip-python2-variant python-tempdir)))

(define-public taxtastic
  (package
    (name "taxtastic")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/t/taxtastic/taxtastic-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1g7fgnl367njdsk2xify9qh20dy63xzamf6w3bi74isgbhykq00h"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python-sqlalchemy" ,python2-sqlalchemy)
       ("python-decorator" ,python2-decorator)
       ("python-biopython" ,python2-biopython)
       ("python-xlrd" ,python2-xlrd)))
    (inputs
     `(("python-setuptools" ,python2-setuptools)))
    (home-page "https://github.com/fhcrc/taxtastic")
    (synopsis
     "Tools for taxonomic naming and annotation")
    (description
     "Tools for taxonomic naming and annotation")
    (license license:gpl3)))

(define-public python2-extern ; could be sent to the mailing list. Does it work
                                        ; with python3 though? Probably, but
                                        ; would need to test the software.
  (package
    (name "python2-extern")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "extern" version))
       (sha256
        (base32
         "01s0bgjjlsm0if3ha5cmz57zp56m2hdqwzcxigq0sy6lsglzkywl"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; current test in setup.py does not work as of 0.9.4,
         ;; so use nose to run tests instead for now.
         (replace 'check (lambda _ (zero? (system* "nosetests")))))))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)
       ("python-nose" ,python2-nose)))
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

(define-public python2-subprocess32
  (package
  (name "python2-subprocess32")
  (version "3.2.6")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/s/subprocess32/subprocess32-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1xi0qb9b70kgwa2ks4d4kkib7dmb9i30rl6zf9rpwb5ys9pd9x6x"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2
     #:tests? #f)) ; no check, and nosetests fails
  (inputs
    `(("python-setuptools" ,python2-setuptools)
      ("python-nose" ,python2-nose)))
  (home-page
    "http://code.google.com/p/python-subprocess32/")
  (synopsis
    "Backport of the subprocess module from Python 3.2/3.3 for use on 2.x.")
  (description
    "Backport of the subprocess module from Python 3.2/3.3 for use on 2.x.")
  (license license:psfl)))


(define-public singlem
  (package
    (name "singlem")
    (version "0.7.0")
    ;; (source
    ;; (local-file "/home/ben/git/singlem/dist/singlem-0.7.0.tar.gz"))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/wwood/singlem/releases/download/v"
                    version "/singlem-" version ".tar.gz"))
              (sha256
               (base32
                "0fka94y832hbxrc9qzk1q341bhvq4crfxj9x5vn726jbsziz822p"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; python-2 only
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-graftm-dependency
           (lambda _
             ;; GraftM 0.9.5 requires scikit-bio 0.2.2, which pulls in
             ;; a bunch of dependencies.  Since there is no released
             ;; version of GraftM after this, do not include it as a
             ;; dependency.
             (substitute* "setup.py"
               (("'graftm >=.*") ""))
             #t))
         ;; (replace 'check
         ;;          (lambda _
         ;;            ;; (zero? (system* "bin/singlem" "--debug" "pipe" "--sequences"
         ;;            ;;                 "bla.fasta" "--otu_table" "stdout"
         ;;            ;;                 "--singlem_packages"
         ;;            ;;                 "test/data/4.11.22seqs.gpkg.spkg"))
         ;;            ;; (system* "cat" "stdout")
         ;;            ;;(zero? (system* "python" "test/test_pipe.py" "Tests.test_fast_protein_package"))))
         ;;            (zero? (system* "nosetests" "-v"))))
         ;;            ;;#t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (graftm (string-append out "/bin/singlem"))
                    (path (getenv "PATH")))
               (wrap-program graftm `("PATH" ":" prefix (,path))))
             #t)))))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)
       ("python-nose" ,python2-nose)))
    (inputs
     `(("graftm" ,graftm)
       ("python-biopython" ,python2-biopython)
       ("python-extern" ,python2-extern)
       ("python-tempdir" ,python2-tempdir)
       ("python-dendropy" ,python2-dendropy)
       ("python-subprocess32" ,python2-subprocess32)
       ("python-biom-format" ,python2-biom-format)
       ("python-h5py" ,python2-h5py)
       ("seqmagick" ,seqmagick)
       ("blast+" ,blast+)
       ("vsearch" ,vsearch)
       ("krona-tools" ,krona-tools)
       ("fxtract" ,fxtract)
       ("hmmer" ,hmmer)
       ("diamond" ,diamond)))
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
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "checkm-genome" version))
     (sha256
      (base32
       "02yhn3rvvhj63gj16hkqqnp3xbd9sikgc7pwpay2jd2vihjkiw4b"))))
   (build-system python-build-system)
   (arguments
    `(#:python ,python-2
      #:phases
      (modify-phases %standard-phases
        (add-after 'build 'set-data-root
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "checkm/DATA_CONFIG"
              (("dataRoot\": \"")
               (string-append
                "dataRoot\": \""
                (assoc-ref inputs
                           "checkm-data"))))))
        (delete 'check)
        (add-after 'install 'set-data-and-check
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (setenv "PYTHONPATH" (string-append
                                  (getenv "PYTHONPATH")
                                  ":" (assoc-ref outputs "out")
                                  "/lib/python"
                                  (string-take (string-take-right
                                                (assoc-ref inputs "python") 5) 3)
                                  "/site-packages"))
            ;; Cannot use the below command due to bug in CheckM
            ;; https://github.com/Ecogenomics/CheckM/issues/58
            ;; (zero?
            ;;  (system* (string-append (assoc-ref outputs "out") "/bin/checkm")
            ;;           "setRoot"
            ;;           (assoc-ref inputs "checkm-data")))

            (zero?
             ;; Use a simple import: the 'test' procedure uses too much RAM.
             ;; (system* (string-append (assoc-ref outputs "out") "/bin/checkm")
             ;;         "test"
             ;;          "checkm_test_results")))))))
             (system* (string-append (assoc-ref outputs "out") "/bin/checkm")
                      "-h")))))))
   (native-inputs
    `(("python2-setuptools" ,python2-setuptools)))
   (inputs
    `(("checkm-data" ,checkm-data)))
   (propagated-inputs
    ;; FIXME: replace calls to hmmer, prodigal and pplacer in the scripts so
    ;; that they can be inputs rather than propagated-inputs.
    `(("hmmer" ,hmmer)
      ("prodigal" ,prodigal)
      ("pplacer" ,pplacer)
      ("python2-numpy" ,python2-numpy)
      ("python2-matplotlib" ,python2-matplotlib)
      ("python2-pysam" ,python2-pysam)
      ("python2-dendropy" ,python2-dendropy)
      ("python2-screaming-backpack" ,python2-screaming-backpack)))
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
    (version "1.0.7")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append
                    "https://data.ace.uq.edu.au/public/CheckM_databases/checkm_data_v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
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
