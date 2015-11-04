import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

  private val counter: AtomicInteger = new AtomicInteger()
  private val uid = generateAccountId
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = ExecutionContext.fromExecutor(new ForkJoinPool())

  Main.thread(processTransactions)

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }
  
  def generateAccountId: Int = {
    counter.incrementAndGet()

  }

  private def processTransactions: Unit = {

    while(!transactionsQueue.isEmpty){
      executorContext.execute(transactionsQueue.pop)
    }
    processTransactions

  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

}
