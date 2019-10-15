#!/bin/perl
#
#Used to prepare the book "tommath.src" for LaTeX by pre-processing it into a .tex file
#
#Essentially you write the "tommath.src" as normal LaTex except where you want code snippets you put
#
#EXAM,file
#
#This preprocessor will then open "file" and insert it as a verbatim copy.
#
#Tom St Denis
use strict;

#get graphics type
my $graph;
if (shift =~ /PDF/) {
   $graph = "";
} else {
   $graph = ".ps";
}

open(my $in, '<', 'tommath.src') or die "Can't open source file";
open(my $out, '>', 'tommath.tex') or die "Can't open destination file";

print "Scanning for sections\n";
my $chapter = 0;
my $section = 0;
my $subsection = 0;
my $x = 0;
my %index1;
my %index2;
my %index3;
while (<$in>) {
   print ".";
   if (!(++$x % 80)) { print "\n"; }
   #update the headings
   if (~($_ =~ /\*/)) {
      if ($_ =~ /\\chapter\{.+}/) {
          ++$chapter;
          $section = $subsection = 0;
      } elsif ($_ =~ /\\section\{.+}/) {
          ++$section;
          $subsection = 0;
      } elsif ($_ =~ /\\subsection\{.+}/) {
          ++$subsection;
      }
   }

   if ($_ =~ m/MARK/) {
      my @m = split ',', $_;
      chomp $m[1];
      $index1{$m[1]} = $chapter;
      $index2{$m[1]} = $section;
      $index3{$m[1]} = $subsection;
   }
}
close $in;

open($in, '<', 'tommath.src') or die "Can't open source file";
my $readline = 0;
my $wroteline = 0;
my $srcline = 0;
my $totlines;
my @text;

while (<$in>) {
   ++$readline;
   ++$srcline;

   if ($_ =~ m/MARK/) {
   } elsif ($_ =~ m/EXAM/ || $_ =~ m/LIST/) {
      my $skipheader;
      if ($_ =~ m/EXAM/) {
         $skipheader = 1;
      } else {
         $skipheader = 0;
      }

      # EXAM,file
      chomp($_);
      my @m = split ',', $_;
      open(my $src, '<', "../$m[1]") or die "Error:$srcline:Can't open source file $m[1]";

      print "$srcline:Inserting $m[1]:";

      my $line = 0;
      my $tmp = $m[1];
      my $fun = $tmp;
      $tmp =~ s/_/"\\_"/ge;
      $fun =~ s/^bn_//;
      $fun =~ s/\.c$//;
      $fun =~ s/_/"\\_"/ge;
      print {$out} "\\index{$fun}\\vspace{+3mm}\\begin{small}\n\\hspace{-5.1mm}{\\bf File}: $tmp\n\\vspace{-3mm}\n\\begin{alltt}\n";
      $wroteline += 5;

      if ($skipheader == 1) {
         # scan till next end of comment, e.g. skip license
         while (<$src>) {
            if ($_ =~ /#ifdef BN/) {
               printf {$out} ("%03d   ", $line);
               for ($x = 0; $x < length($_); $x++) {
                   print {$out} chr(vec($_, $x, 8));
                   if ($x == 75) {
                       print {$out} "\n      ";
                       ++$wroteline;
                   }
               }
               print {$out} "...\n";
               ++$wroteline;
            }
            $text[$line++] = $_;
            last if ($_ =~ /libtom\.org/);
         }
         <$src>;
         $text[$line++] = $_;
         <$src>;
         $text[$line++] = $_;
      }

      my $inline = 0;
      while (<$src>) {
      next if ($_ =~ /ref/);
      next if ($_ =~ /git commit/);
      next if ($_ =~ /commit time/);
         $text[$line++] = $_;
         ++$inline;
         chomp($_);
         $_ =~ s/\t/"    "/ge;
         $_ =~ s/{/"^{"/ge;
         $_ =~ s/}/"^}"/ge;
         $_ =~ s/\\/'\symbol{92}'/ge;
         $_ =~ s/\^/"\\"/ge;

         printf {$out} ("%03d   ", $line);
         for ($x = 0; $x < length($_); $x++) {
             print {$out} chr(vec($_, $x, 8));
             if ($x == 75) {
                 print {$out} "\n      ";
                 ++$wroteline;
             }
         }
         print {$out} "\n";
         ++$wroteline;
      }
      $totlines = $line;
      print {$out} "\\end{alltt}\n\\end{small}\n";
      close $src;
      print "$inline lines\n";
      $wroteline += 2;
   } elsif ($_ =~ m/@\d+,.+@/) {
     # line contains [number,text]
     # e.g. @14,for (ix = 0)@
     my $txt = $_;
     while ($txt =~ m/@\d+,.+@/) {
        my @m = split '@', $txt;        # splits into text, one, two
        my @parms = split ',', $m[1];   # splits one,two into two elements

        # now search from $parms[0] down for $parms[1]
        my $found;
        my $found1 = 0;
        my $found2 = 0;
        my $foundline;
        my $foundline1;
        my $foundline2;
        for (my $i = $parms[0]; $i < $totlines && $found1 == 0; $i++) {
           if ($text[$i] =~ m/\Q$parms[1]\E/) {
              $foundline1 = $i + 1;
              $found1 = 1;
           }
        }

        # now search backwards
        for (my $i = $parms[0] - 1; $i >= 0 && $found2 == 0; $i--) {
           if ($text[$i] =~ m/\Q$parms[1]\E/) {
              $foundline2 = $i + 1;
              $found2 = 1;
           }
        }

        # now use the closest match or the first if tied
        if ($found1 == 1 && $found2 == 0) {
           $found = 1;
           $foundline = $foundline1;
        } elsif ($found1 == 0 && $found2 == 1) {
           $found = 1;
           $foundline = $foundline2;
        } elsif ($found1 == 1 && $found2 == 1) {
           $found = 1;
           if (($foundline1 - $parms[0]) <= ($parms[0] - $foundline2)) {
              $foundline = $foundline1;
           } else {
              $foundline = $foundline2;
           }
        } else {
           $found = 0;
        }

        # if found replace
        if ($found == 1) {
           my $delta = $parms[0] - $foundline;
           print "Found replacement tag for \"$parms[1]\" on line $srcline which refers to line $foundline (delta $delta)\n";
           $_ =~ s/@\Q$m[1]\E@/$foundline/;
        } else {
           print "ERROR:  The tag \"$parms[1]\" on line $srcline was not found in the most recently parsed source!\n";
        }

        # remake the rest of the line
        $txt = "";
        for (my $i = 2; $i < scalar(@m); $i++) {
            $txt = $txt . $m[$i] . "@";
        }
     }
     print {$out} $_;
     ++$wroteline;
   } elsif ($_ =~ /~.+~/) {
      # line contains a ~text~ pair used to refer to indexing :-)
      my $txt = $_;
      while ($txt =~ /~.+~/) {
         my @m = split '~', $txt;

         # word is the second position
         my $word = $m[1];
         my $a = $index1{$word};
         my $b = $index2{$word};
         my $c = $index3{$word};

         # if chapter (a) is zero it wasn't found
         if ($a == 0) {
            print "ERROR: the tag \"$word\" on line $srcline was not found previously marked.\n";
         } else {
            # format the tag as x, x.y or x.y.z depending on the values
            my $str = $a;
            $str = $str . ".$b" if ($b != 0);
            $str = $str . ".$c" if ($c != 0);

            if ($b == 0 && $c == 0) {
               # its a chapter
               if ($a <= 10) {
                  if ($a == 1) {
                     $str = "chapter one";
                  } elsif ($a == 2) {
                     $str = "chapter two";
                  } elsif ($a == 3) {
                     $str = "chapter three";
                  } elsif ($a == 4) {
                     $str = "chapter four";
                  } elsif ($a == 5) {
                     $str = "chapter five";
                  } elsif ($a == 6) {
                     $str = "chapter six";
                  } elsif ($a == 7) {
                     $str = "chapter seven";
                  } elsif ($a == 8) {
                     $str = "chapter eight";
                  } elsif ($a == 9) {
                     $str = "chapter nine";
                  } elsif ($a == 10) {
                     $str = "chapter ten";
                  }
               } else {
                  $str = "chapter " . $str;
               }
            } else {
               $str = "section " . $str     if ($b != 0 && $c == 0);
               $str = "sub-section " . $str if ($b != 0 && $c != 0);
            }

            #substitute
            $_ =~ s/~\Q$word\E~/$str/;

            print "Found replacement tag for marker \"$word\" on line $srcline which refers to $str\n";
         }

         # remake rest of the line
         $txt = "";
         for (my $i = 2; $i < scalar(@m); $i++) {
             $txt = $txt . $m[$i] . "~";
         }
      }
      print {$out} $_;
      ++$wroteline;
   } elsif ($_ =~ m/FIGU/) {
      # FIGU,file,caption
      chomp($_);
      my @m = split ',', $_;
      print {$out} "\\begin{center}\n\\begin{figure}[h]\n\\includegraphics{pics/$m[1]$graph}\n";
      print {$out} "\\caption{$m[2]}\n\\label{pic:$m[1]}\n\\end{figure}\n\\end{center}\n";
      $wroteline += 4;
   } else {
      print {$out} $_;
      ++$wroteline;
   }
}
print "Read $readline lines, wrote $wroteline lines\n";

close $out;
close $in;

system('perl -pli -e "s/\s*$//" tommath.tex');
