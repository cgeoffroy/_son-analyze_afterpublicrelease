FROM jupyter/base-notebook

### Warning: this Dockerfile's context is the root directory of the project

USER root

RUN apt-get update \
    # Install common packages, Ansible and Git
    && apt-get install -y software-properties-common apt-transport-https ansible git \
    # Add the localhost to the Ansible's hosts
    && echo 'localhost ansible_connection=local' >> /etc/ansible/hosts \
    && apt-get install -y libyaml-dev \
    # Pre-install python 3.5 and pip3 to speed-up the next steps
    #&& apt-get install -y python3.5 python3-pip libyaml-dev \
    && echo 'Done'

WORKDIR /son-analyze

COPY . /son-analyze

RUN cd /son-analyze/ansible \
    # Start the basic Ansible setup
    && ansible-playbook install.yml \
    && cd /son-analyze \
    && ./scripts/clean.sh \
    && pip3 install -r requirements.txt \
    && echo 'Local install' \
    && python setup.py develop \
    && echo 'Done'

USER jovyan
