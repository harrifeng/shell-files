#! /bin/bash
cd ~/ct
pwd
which rake
echo "--------------------------------------------------------"
bundle exec rake db:create:all
echo "--------------------------------------------------------"
bundle exec rake db:schema:load
echo "--------------------------------------------------------"
bundle exec rake db:migrate
echo "--------------------------------------------------------"
bundle exec rake db:populate
echo "--------------------------------------------------------"
bundle exec rake cstar:init
echo "--------------------------------------------------------"
bundle exec rake feature_sets:init
echo "--------------------------------------------------------"
bundle exec rake feature_sets:update
echo "--------------------------------------------------------"
bundle exec rake templates:groups:invalidate_preview_and_init
echo "--------------------------------------------------------"
bundle exec rake events:categories:update
echo "--------------------------------------------------------"
