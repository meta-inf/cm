set -e

cat ~/.bashrc | grep export > ~/.bashrc.slv  || touch ~/.bashrc.slv
source ~/.bashrc.slv

type conda || {
	set -e
	echo "Conda not found"
	wget https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/Anaconda3-5.0.1-Linux-x86_64.sh
	chmod +x Anaconda3-5.0.1-Linux-x86_64.sh
	bash Anaconda3-5.0.1-Linux-x86_64.sh -b
}

cat ~/.bashrc | grep export > ~/.bashrc.slv  || touch ~/.bashrc.slv
echo "export LD_LIBRARY_PATH=/opt/cudnn-v7/lib64:$LD_LIBRARY_PATH" >> ~/.bashrc.slv
echo "export PATH=/usr/local/cuda-9.0/bin:$PATH" >> ~/.bashrc.slv
echo "export CPATH=/opt/cudnn-v7/include:$CPATH" >> ~/.bashrc.slv
source ~/.bashrc.slv

conda env list | grep "myenv" || {
	set -e
	yes | conda create -n myenv python=3.6 pip
	source activate myenv
	pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
	pip install tensorflow-gpu==1.11
}

echo "done"
