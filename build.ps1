

if ((Test-Path ".\.paket") -eq $False){
    New-Item -Type Directory .paket
    }

cp "C:\Users\Me\Documents\GitProjects\Paket\bin\*" .\.paket

.paket/paket.bootstrapper.exe