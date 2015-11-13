$_ = "not ok $test - s/// in s/// pattern\n";
s/${s|||;\""}not //;
print; $test++;

