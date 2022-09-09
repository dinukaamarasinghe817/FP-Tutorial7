object question4 extends App{

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

    // filter only overdraft accounts
    val overdraft = (x:List[Account]) => x.filter(account => account.balance < 0.0)

    // calculate the toatal balnce of all accounts
    // val totalBalance = (x:List[Account]) => x.foldLeft(0.0)((x, y) => x + y.balance)
    def totalBalance(x:List[Account]):Double = if(x.length == 0) 0.0 else x.head.balance + totalBalance(x.tail)

    // val interest = (x:List[Account]) => x.map(account => if(account.balance > 0) account.balance*1.05 else account.balance*1.1)
    val applyinterest = (x:List[Account]) => x.map(account => if(account.balance > 0) new Account(account.ID,account.balance*1.05) else new Account(account.ID,account.balance*1.1))

    // create two new accounts
    accCreate(1)
    accCreate(2)

    //deposit money
    find(1, accountList)(0).deposit(1000)
    println(find(1, accountList)(0))

    //transfer money
    find(1, accountList)(0).transfer(2, 10.0)
    find(1, accountList)(0).transfer(2, 1000.0)

    // finding overdraft accounts
    println("Overdraft accounts :")
    println(overdraft(accountList))

    // finding total balance of all accounts
    println("\n total balance: " +totalBalance(accountList))

    // apply the interest rate to every account
    accountList = applyinterest(accountList)
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

        override def toString(): String = "\nAcocunt ID : "+ID+", balance :"+balance+"\n"
    }
}