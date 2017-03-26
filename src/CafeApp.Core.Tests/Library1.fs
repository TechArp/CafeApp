namespace Cafe.Core.Tests

open CafeApp.Core
open NUnit.Framework


type Class1() =
    member this.X = "F#"


module UnitTests =
    [<Test>]
    let ``Sample Test``() =
        let class1 = new Class1()
        Assert.AreEqual("F#", class1.X)
