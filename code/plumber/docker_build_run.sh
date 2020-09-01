

sudo docker build --tag auto-premium -f code/plumber/API/Dockerfile code/plumber/API
sudo docker run -d -p 8000:8000 --name auto1 auto-premium


sudo docker stop auto1
sudo docker rm auto1

