{ pkgs, ... }:

let

  network-create = pkgs.writeShellScriptBin "network-create" ''
    docker network create jenkins
  '';

  gitlab-start = pkgs.writeShellScriptBin "gitlab-start" ''
    export GITLAB_HOME=$HOME/gitlab
    docker run --detach \
      --hostname gitlab.example.com \
      --network jenkins \
      --publish 11443:443 --publish 1180:80 --publish 1122:22 \
      --name gitlab \
      --restart always \
      --volume $GITLAB_HOME/config:/etc/gitlab \
      --volume $GITLAB_HOME/logs:/var/log/gitlab \
      --volume $GITLAB_HOME/data:/var/opt/gitlab \
      --shm-size 256m \
      gitlab/gitlab-ee:latest
  '';

  gitlab-initial-password = pkgs.writeShellScriptBin "gitlab-initial-password" ''
    docker exec -it gitlab grep 'Password:' /etc/gitlab/initial_root_password
  '';

  jenkins-dind-start = pkgs.writeShellScriptBin "jenkins-dind-start" ''

    docker run --name jenkins-docker --rm --detach \
      --privileged --network jenkins --network-alias docker \
      --env DOCKER_TLS_CERTDIR=/certs \
      --volume jenkins-docker-certs:/certs/client \
      --volume jenkins-data:/var/jenkins_home \
      --publish 2376:2376 \
      docker:dind --storage-driver overlay2
  '';

  jenkins-container-build = pkgs.writeShellScriptBin "jenkins-container-build" ''
    docker build -t myjenkins ${./jenkins}
  '';

  jenkins-start = pkgs.writeShellScriptBin "jenkins-start" ''
    docker run --name jenkins --restart=on-failure --detach \
      --network jenkins --env DOCKER_HOST=tcp://docker:2376 \
      --env DOCKER_CERT_PATH=/certs/client --env DOCKER_TLS_VERIFY=1 \
      --publish 8080:8080 --publish 50000:50000 \
      --volume jenkins-data:/var/jenkins_home \
      --volume jenkins-docker-certs:/certs/client:ro \
      myjenkins
  '';

in

{

  imports = [
    ../../modules/docker.nix
  ];

  networking.firewall.allowedTCPPorts = [ 6443 ];
  services.k3s.enable = true;
  services.k3s.role = "server";
  services.k3s.extraFlags = toString [
    # "--kubelet-arg=v=4" # Optionally add additional args to k3s
  ];

  environment.systemPackages = [
    pkgs.k3s
    pkgs.kubernetes-helm
    network-create
    gitlab-start
    gitlab-initial-password
    jenkins-dind-start
    jenkins-container-build
    jenkins-start
  ];

}
