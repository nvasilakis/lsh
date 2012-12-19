# λ

 **lambda** *|ˈlamdə|*

noun  
the eleventh letter of the Greek alphabet (Λ, λ), transliterated as ‘l.’  
• (Lambda) [ followed by Latin genitive ] `Astronomy` the eleventh star in a constellation: *Lambda Tauri*.  
• `Biology` a type of bacteriophage virus used in genetic research: [ as modifier ] : *lambda phage*.  
• `Anatomy` the point at the back of the skull where the parietal bones and the occipital bone meet.  
• [ as modifier ] `Biochemistry` denoting one of the two types of light polypeptide chain present in all immunoglobulin molecules (the other being kappa).  

symbol  
• (λ) wavelength.  
• (λ) Astronomy celestial longitude.  

# lamda shell

lsh is a  new shell focused in bridging  functional programming concepts  
into one of the most traditional (yet powerful) computer interfaces ever  
made  --  the shell.  The  shell  stands  as  an interface  between  the  
programmer  and indispensable  tools, the  system administrator  and the  
tasks he  has to  accomplish, the  power user and  the control  over his  
machine.  

# authors

Chongyu Chen        (chongyu@seas.upenn.edu)  
Nikolaos Vasilakis  (nvas@seas.upenn.edu)  
Saurabh Garg        (saurabhg@seas.upenn.edu)  

# repository

http://github.com/nvasilakis/lsh

# files

lsh  uses  Cabal  for  building  and  packaging  Haskell  libraries  and  
programs. This way  users can leverage the common  interface for package  
authors  and  distributors  to  easily  build lsh  in  a  portable  way,  
administer dependencies etc.  

Nevertheless, we list the modules our project is comprised of:

 ConfigFile.hs    -- parsing code for the .lshrc configuration file  
 Evaluator.hs     -- evaluation of parsed input  
 Help.hs          -- contains hardcoded strings, mostly for testing  
 Main.hs          -- entry point for the project, incl. interactive and non-interactive mode  
 Parser.hs        -- parsing of user input  
 Setup.hs         -- Cabal setup file  
 Tests.hs         -- Testing code, (added to pre-commit hook)  
 Types.hs         -- datatype definitions, accessors   
 lsh.cabal        -- commented cabal file, listing dependencies, license etc.  
  
 README.md        -- This README file, often containing obsolete info.  
 LICENSE          -- BSD license  
