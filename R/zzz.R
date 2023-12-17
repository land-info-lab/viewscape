#' @import Rcpp

.onLoad <- function(libname, pkgname) {
  suppressWarnings(
    Rcpp::sourceCpp(system.file("extdata/visibleLabel.cpp", package ="viewscape"),
                    env = asNamespace("viewscape"))
  )
  suppressWarnings(
    Rcpp::sourceCpp(system.file("extdata/multiLabel.cpp", package ="viewscape"),
                    env = asNamespace("viewscape"))
  )
  suppressWarnings(
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    if (!requireNamespace("BiocParallel", quietly = TRUE)) {
      BiocManager::install("BiocParallel")
    }
  )

  # cat("████████████████████████████████████████████████████████████████████████████████████████████████████\n")
  # cat("████████████████████████████████████████████████████████████████████████████████████████████████████\n")
  # cat("██████████████████████████████████████████████MM/MMM/MMM/M///M██████████████████████████████████████\n")
  # cat("███████████████████████████████████████/M////MM/M/MM//////MM//MM///MM///M███████████████████████████\n")
  # cat("███████████████████████████████/M////MM//MM/MM/MMMM//MMM///MM///MM/////MM//MM/██████████████████████\n")
  # cat("██████████████████████████/////MM/MM/M/MMM/mn=======nmMMMM/M/MM/////MMM///MMM/M█████████████████████\n")
  # cat("█████████████████████////MM/MMMMMMmn-=====nMMMMMMMMMMn=====nmMMMMMM///MMM//MMM////M/████████████████\n")
  # cat("██████████████████///M/MMMMmm-=====nmMMMMm::::...::::NMMMn-=======nmMM██████████████████████████████\n")
  # cat("███████████████//MMMMMMm-====nMMMN:.                 ..:::::MMMMn-======nmMM████████████████████████\n")
  # cat("████████████///MMMM-===nMMN:::..      ////XXXooo        ...::::::::NMMn-====nmMM████████████████████\n")
  # cat("███████████///MM-==MMn:::..         ////XX??-||-oo             ...:::::::::NMM-===MM████████████████\n")
  # cat("████████████-===MNn:..            ///XXXX??------oo             ..........::::MMM==MM███████████████\n")
  # cat("█████████████==:...              iiii'''ii??----oooo           ........::::MMM-===MM████████████████\n")
  # cat("██████████████==:.              iii'''''iiiii////oooo        ........:::MMMM-===MM██████████████████\n")
  # cat("████████████████==:..           iii''''''iii///oooo       .......:::::MMM-===MM█████████████████████\n")
  # cat("██████████████████==::.          iii'''''ooo//ooooo      ...::::::MMMM-==MM█████████████████████████\n")
  # cat("█████████████████████==::..       iioo'''''//ooooo   ..::::::MMMM-==MM██████████████████████████████\n")
  # cat("█████████████████████████==:::......ioooooooooo....:::::MMMMMM==MM██████████████████████████████████\n")
  # cat("███████████████████████████████████MMMmmmmmmmmmmMMM█████████████████████████████████████████████████\n")
  # cat("████████████████████████████████████████████████████████████████████████████████████████████████████\n")
  # cat("████████████████████████████████████████████████████████████████████████████████████████████████████\n")
  # cat("█::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::█\n")
  # cat("█::██:::::::::██::██::██████::██::::██::::██::████████:::██████:::::███::::::::█████████:::██████::█\n")
  # cat("█:::██:::::::██::██::██::::::██:::████:::██::██:::::::::██::::██:::██:██::::::██::::::██::██:::::::█\n")
  # cat("█::::██:::::██::██::██::::::██::██::██::██::████████:::██:::::::::██::██:::::██::::::██::██::::::::█\n")
  # cat("█:::::██:::██::██::██████::██:██::::██:██:::::::::██::██:::::::::███████::::█████████::███████:::::█\n")
  # cat("█::::::██:██::██::██::::::███:::::::███:::██::::███::██:::::██::██::::██:::██:::::::::██:::::::::::█\n")
  # cat("█:::::::███::██::██████::██:::::::::██:::█████████:::████████::██:::::██::██:::::::::███████:::::::█\n")
  # cat("█::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::█\n")
  # cat("████████████████████████████████████████████████████████████████████████████████████████████████████\n")
  # cat("VIEWSCAPE_0.2.0\n")
  invisible()
}

.onUnload <- function(libpath)
{
  library.dynam.unload("viewscape", libpath)
}

#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM/MMM/MMM/M///MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMM/M////MM/M/MM//////MM//MM///MM///MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMM/M////MM//MM/MM/MMMM//MMM///MM///MM/////MM//MM/MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMM/////MM/MM/M/MMM/mn=======nmMMMM/M/MM/////MMM///MMM/MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMM////MM/MMMMMMmn-=====nMMMMMMMMMMn=====nmMMMMMM///MMM//MMM////M/MMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMM///M/MMMMmm-======nMMMMm:::::::::::NMMMn-=======nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMM//MMMMMMm-====nMMMN::::::..............::::::MMMMn-======nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MM///MMMM-===nMMN:::::::....////XXXooo...........::::::::NMMn-====nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMM-==MMn:::...........////XX??-||-oo................:::::::::NMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMM
#MM-===MNn:..............///XXXX??------oo.......................::::MMM==MMMMMMMMMMMMMMMMMMMMMMMMMM
#MMM==:.................iiii'''ii??----oooo...................::::MMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMM==:...............iii'''''iiiii////oooo................:::MMMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMM==:::............iii''''''iii///oooo..............:::::MMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMM==::::.........iii'''''ooo//ooooo.........::::::MMMM-==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMM==::::::.....iioo'''''//ooooo.....::::::MMMM-==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMM==:::::::..ioooooooooo:::::::::MMMMMM==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMmmmmmmmmmmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MM..MMMMMMMMM..MM..MM......MM..MMMM..MMMM..MM.......MMM.......MMMMM...MMMMM.........MMMM......MMMMM
#MMM..MMMMMMM..MM..MM..MMMMMM..MM..M..MMM..MM..MMMMMMMM..MMMMM..MMM..M..MMMM..MMMMMM..MM..MMMMMMMMMM
#MMMM..MMMMM..MM..MM..MMMMMM..MM..MM..MM..MM........MM..MMMMMMMMMM..MM..MMM..MMMMMM..MM..MMMMMMMMMMM
#MMMMM..MMM..MM..MM......MM..M..MMMM..M..MMMMMMMMM..MM..MMMMMMMMM.......MMM.........MM.......MMMMMMM
#MMMMMM..M..MM..MM..MMMMMM...MMMMMMM...MMM..MMMM...MM...MMMM..MM..MMMM..MMM..MMMMMMMM..MMMMMMMMMMMMM
#MMMMMMM...MM..MM......MM..MMMMMMMMM..MMM.........MMM........MM..MMMMM..MM..MMMMMMMM.......MMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

