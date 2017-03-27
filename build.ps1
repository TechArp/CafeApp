
Param(
    [Parameter(Position=0)]
    [ValidateSet('Clean','BuildApp','RunUnitTests')]
    [System.String]$Job
)


if ((Test-Path ".\.paket") -eq $False){
    New-Item -Type Directory .paket
    }

cp "C:\Users\Me\Documents\GitProjects\Paket\bin\*" .\.paket

.paket/paket.bootstrapper.exe
.paket/paket.exe restore
.\packages\FAKE\tools\FAKE.exe --fsiargs build.fsx