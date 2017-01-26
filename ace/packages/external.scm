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
  #:use-module (gnu packages zip)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

;;; Building from source is difficult given the package is built from an
;;; outdated version of OCaml so we cannot submit this to be a part of Guix
;;; proper just yet.
(define-public pplacer
  (package
    (name "pplacer")
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
