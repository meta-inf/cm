set -e

type conda || {
	echo "Conda not found"
	wget https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/Anaconda3-5.0.1-Linux-x86_64.sh
	chmod +x Anaconda3-5.0.1-Linux-x86_64.sh
	bash Anaconda3-5.0.1-Linux-x86_64.sh -b
}

cat ~/.bashrc | grep export > ~/.bashrc.slv  || touch ~/.bashrc.slv
source ~/.bashrc.slv

conda env list | grep "myenv" || {
	yes | conda create -n myenv python=3.6 pip
	source activate myenv
	pip install tensorflow-gpu==1.11
}

echo "done"
