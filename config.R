# randomization
SEED                = 1337

# system config
THREADS             = 6

# logging system
LOGGER.OUTPUT.DIR               = "logs"
LOGGER.OUTPUT.TEST.FILE         = "output-test.log"
LOGGER.LEVEL                    = 6 # futile.logger::INFO
LOGGER.OVERWRITE.EXISTING.FILES = TRUE


DATASETS.DIR           = 'datasets'
EVALUATION.OUTPUT.FILE = 'evaluation-output.RData'

EVALUATION.OUTPUT.LOCATION = paste(DATASETS.DIR, EVALUATION.OUTPUT.FILE, sep='/')

GEN.DATA.URL = "http://min.wmi.amu.edu.pl/data/ovarian-tumor-aggregation"

set.seed(SEED)

if (file.exists("config.R.user"))
    source("config.R.user")
