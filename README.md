## ACE Guix package repository
This is a colleciton of [GNU Guix](https://www.gnu.org/software/guix/) packages to make installation simpler. It is 
particularly aimed at software produced by the [Australian Centre for Ecogenomics](http://ecogenomic.org/), but may
be used for other packages as well.

To use this repository, you'll need to install Guix on a Linux machine as per usual method,
and then use the ACE repository like so:
```
git clone https://github.com/Ecogenomics/ace-guix
GUIX_PACKAGE_PATH=ace-guix guix package -i dirseq ruby
```
Then you'll have have `dirseq` in your profile, including dependencies such as `bedtools` and `ruby`.
If you have not done so already, you'll need to modify your environment variables
as instructed.

### Software available
* `dirseq` is a metatranscriptome-target gene coverage calculator

##License
The ACE Guix repository is available under GPL version 3 or later.
