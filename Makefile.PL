use inc::Module::Install 0.67;
perl_version '5.008001';
  
name        'Devel-REPL-Plugin-NAS';
all_from    'lib/Devel/REPL/Plugin/NAS.pm';

install_script 'nsh';

requires    'Devel::REPL' => 0;
requires    'Net::Appliance::Session' => 1.23;

test_requires  'Test::More' => 0;
test_requires  'Test::Pod' => 0;
test_requires  'Test::Pod::Coverage' => 0;

WriteAll;

