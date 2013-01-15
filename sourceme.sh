
export ROOT=$(pwd)
# Hook up with CUDA
export LD_LIBRARY_PATH="/usr/local/cuda-5.0/lib64"
export PATH="$PATH:/usr/local/cuda-5.0/bin"

# fetching arguments

source surveytools/bin/loadHSENV
