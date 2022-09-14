import rpy2.robjects as robjects
import logging as logger
import sys

logger.getLogger("robyn_v02 logger")
logger.basicConfig(level=logger.DEBUG,
                    #filename=os.path.basename(__file__) + '.log',
                    format="{asctime} [{levelname:8}] {process} {thread} {module}: {message}",
                    style="{",
                   stream=sys.stdout)


try:
    r = robjects.r
    ng_out= r.source('nevergrad_seqeuntial_bernardo.R')
    print("SUCCESS")
except:
    logger.exception("ERROR")
