


if ((Test-Path ".\.projekt") -eq $False){
    New-Item -Type Directory .projekt
    }

cp "C:\Users\Me\Documents\GitProjects\Projekt\src\Projekt\bin\Debug\*" .\.projekt


