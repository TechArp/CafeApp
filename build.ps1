

if ((Test-Path ".\.paket") -eq $False){
    New-Item -Type Directory .paket
    }

cp "C:\Users\Me\Documents\GitProjects\Paket\bin\*" .\.paket

.paket/paket.bootstrapper.exe
.paket/paket.exe restore
.\packages\FAKE\tools\FAKE.exe --fsiargs --RunUnitTests build.fsx