## ACE Guix package repository
This is a colleciton of [GNU Guix](https://www.gnu.org/software/guix/) packages to make installation simpler. It is 
particularly aimed at software produced by the [Australian Centre for Ecogenomics](http://ecogenomic.org/), but may
be used for other genomics/metagenomics -related packages as well. Contributions are most welcome!

To use this repository, you'll need to install Guix on a Linux machine as per the usual method described in the [Guix manual](https://www.gnu.org/software/guix/manual/),
and then use the ACE repository like so:
```
git clone https://github.com/Ecogenomics/ace-guix
GUIX_PACKAGE_PATH=ace-guix guix package -i dirseq ruby
```
Then you'll have have `dirseq` in your profile, including dependencies such as `bedtools` and `ruby`.
If you have not done so already, you'll need to modify your environment variables
as instructed.

### Software available
* [dirseq](https://github.com/wwood/dirseq) is a metatranscriptome-target gene coverage calculator

### ACE Software available in the main Guix repository
For some packages this repository is not needed as the software can be installed by the usual method in Guix (better there than here)  e.g. one can simply
```
guix pacakge -i orfm
```
* [OrfM](https://github.com/wwood/OrfM) is a fast open reading frame finder for contigs and reads.

##License
The ACE Guix repository is available under GPL version 3 or later.
