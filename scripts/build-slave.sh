# build the AppImage on a system with sufficiently old glibc
# running out of space on the local machine so I can't do it in a fancier way ..
set -e 
cd $(dirname "$0")
scp on-bm.sh j2:/tmp
ssh j2 "bash /tmp/on-bm.sh"
scp j2:cm/slave-exe-x86_64.AppImage ../assets/
