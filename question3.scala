object question3 extends App{
    // list of accounts
    var accountList:List[Account] = List()

    // method to create a new account
    // and add it to the list of accounts
    def accCreate(accId: Int):Unit = {
        val acc = new Account(accId)
        accountList = accountList ::: acc :: Nil
    }

    // find function as a lambda function
    val find = (a:Int, b:List[Account]) => b.filter(account => account.ID.equals(a))
    
    // create two new accounts
    accCreate(1)
    accCreate(2)

    //deposit money to first account
    find(1, accountList)(0).deposit(1000)
    println(find(1, accountList)(0)) // since it retruns an array, used 0 index to access first account

    //transfer money to second account from first account
    find(1, accountList)(0).transfer(2, 100.0)
    println(find(2, accountList)(0))
    println(find(1, accountList)(0))

    // account class
    class Account(val ID:Int, var balance:Double = 0.0){
        // method to withdraw from the account
        def withdraw(amount:Double):Unit = {
            this.balance -= amount
        }
        // method to deposit to the account
        def deposit(amount:Double):Unit = {
            this.balance += amount
        }

        // method to transfer from one account to another account
        def transfer(accID:Int, amount:Double):Unit = {
            this.withdraw(amount)
            find(accID,accountList)(0).deposit(amount)
        }

        override def toString(): String = "\nAcocunt ID : "+ID+", balance :"+balance
    }
}