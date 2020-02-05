package game

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.util.Random


/**
 * Represents a Card suit - Hearts, Diamonds, Spades, Clubs
 */
sealed trait Suit
case object Heart extends Suit
case object Diamond extends Suit
case object Spade extends Suit
case object Club extends Suit


/**
 * Represents a Card rank - Ace, King, Queen, Jack, 10..1
 */
sealed abstract class Rank(val value: Int)
case object King extends Rank(10)
case object Queen extends Rank(10)
case object Jack extends Rank(10)
case object Ten extends Rank(10)
case object Nine extends Rank(9)
case object Eight extends Rank(8)
case object Seven extends Rank(7)
case object Six extends Rank(6)
case object Five extends Rank(5)
case object Four extends Rank(4)
case object Three extends Rank(3)
case object Two extends Rank(2)
case object Ace extends Rank(1)

/**
 * Represents a card
 */
case class Card(suit: Suit, rank: Rank)

/**
 * Represents a deck of cards
 */
class Deck {

  val cards: mutable.Buffer[Card] = Random.shuffle(initDeck).toBuffer

  /**
   * Method that deals a card from the deck.
   * The top card is removed from the deck and returned
   */
  def dealCard: Option[Card] =
    if (cards.isEmpty) {
      None
    } else {
      val card = cards.head
      cards -= card
      Some(card)
    }

  /**
   * Method that initializes the deck of cards
   */
  def initDeck: Seq[Card] =
    for {
      suit <- List(Heart, Diamond, Spade, Club)
      rank <- List(King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Ace)
    } yield Card(suit, rank)
}


/**
 * Represents a Hand of cards
 * Used for both the player and dealer
 */
class Hand {

  val winningValue = 21

  /**
   * The buffer to hold the cards.
   */
  val cards: mutable.Buffer[Card] = ArrayBuffer[Card]()

  /**
   * Function to determine if the hand has an Ace
   */
  def containsAce(): Boolean = cards.exists(c => c.rank == Ace)

  /**
   * Method that adds a card to the existing hand
   */
  def addCard (c: Card): Unit = cards += c

  /**
   * Function that computes the value of the hand
   *
   * Hint: a fold is needed, of some kind
   */
  def value(): Int = cards.foldLeft(0)((result, card) => result + card.rank.value)

  /**
   * Function that computes the value of the hand with an ace valued at 11 instead of 1
   *
   * Hint: use the aforementioned containsAce method to check if the hand contains an ace
   */
  def specialValue(): Int = if (containsAce()) value + 10 else value()

  /**
   * Checks if this hand has winning cards
   *
   * Hint: there are two ways to get blackjack.
   */
  def isBlackJack: Boolean = value() == winningValue || specialValue() == winningValue

  /**
   * Checks if this hand has lost
   */
  def isBust: Boolean = value() > winningValue

  /**
   * Checks if this hand wins over the hand specified
   *
   * Hint: compare max possible value for both hands
   */
  def winsOver (otherHand: Hand): Boolean = {
    val opponentBestValue = List(otherHand.value(), otherHand.specialValue()).filter(v => v <= winningValue).max
    val bestValue = List(value(), specialValue()).filter(v => v <= winningValue).max
    // Return true if all of this hand's best value is higher than the other hand's best value
    bestValue > opponentBestValue
  }

  /**
   * Return the cards in the hand
   * If the dealer flag is true, only the first card is shown
   */
  def showCards (dealer: Boolean = false): String =
    if (dealer) s"${cards.head.rank.value} X" else cards.map(c => c.rank.value) mkString ", "
}

/**
 * Main class to start the Blackjack single player game
 * User starts with a 100 coin credit
 */
class BlackJack (var userCredit: Int = 100) {
  // Main program execution begins
  BlackJack.printRules()

  /**
   * Method that starts the game
   */
  def start(): Unit = {
    val startCredit = userCredit
    var continueExecution = true
    val gameSummary = collection.mutable.ArrayBuffer[String]()
    var currHand = 0

    // Game ends when the user has exhausted all the coins or chooses to quit the game
    while (continueExecution) {
      addNewLine()
      log("---------------- Starting New Hand ----------------")
      val credit = betForHand()
      // Reduce the existing user credit by currently waged bet if he loses
      // Add the waged bet to available credit if he wins
      val (resultCredit: Int, playerHand: Hand, dealerHand: Hand) = playHand(credit, new Deck)
      userCredit += resultCredit
      val result = if (resultCredit > 0) "Won" else "Lost"
      currHand += 1
      gameSummary += s"Round: $currHand, Result: $result, Player: [ ${playerHand.showCards()} ], Dealer: [ ${dealerHand.showCards()} ]"
      continueExecution = continueGame()

    }
    log(s"$newLine======= Game Summary =======")
    log(s"Start-Credit: [ $startCredit ],  End-Credit: [ $userCredit ]$newLine")
    gameSummary foreach log
  }

  /**
   * Function to retrieve the hand's bet from user
   */
  def betForHand(): Int = {
    var credit = userCredit + 1 // initialize credit to be more than the current credit to keep the loop below going
    while (credit > userCredit || credit < 1) {
      log(s"${newLine}Please enter your bet for this round. Credit at hand: [ $userCredit ]$newLine")
      credit = readInt()
    }
    // Return the bet for this hand
    credit
  }

  /**
   * Function that asks user if game should be continued based on user credit
   */
  def continueGame(): Boolean = {
    // If user doesn't have any credit, we don't continue
    var input = if (userCredit > 0) "" else "N"
    val responses = List("Y", "N", "n", "y")
    // Loop till user enters either "Y" or "N"
    while (!responses.contains(input)) {
      input = readLine("Continue the game? Enter one of (Y,y,N,n): ")
    }
    // Check if user wants to continue
    "Y".equalsIgnoreCase(input)
  }

  /**
   * Function that plays the round
   * Returns a positive credit if the player wins and a negative credit if the dealer wins
   *
   * Hint: use didPlayerWin
   */
  def playHand(credit: Int, deck: Deck): (Int, Hand, Hand)= {
    log(s"Bet for the current hand: ( $credit )$newLine$newLine")
    val (playerHand: Hand, dealerHand: Hand) = dealCards(deck)
    var result = credit
    // After the cards are dealt, a hand with 21 wins
    // If there is a tie, the dealer wins
    if (dealerHand.isBlackJack) {
      showCards(playerHand, dealerHand, maskDealerCards = false)
      log("*** BlackJack: Dealer wins! ***")
      result = -credit
    } else if (playerHand.isBlackJack) {
      showCards(playerHand, dealerHand, maskDealerCards = false)
      log("*** BlackJack: Player wins! ***")
      result = credit
    } else {
      val playerWon = didPlayerWin(deck, playerHand, dealerHand)
      result = if (playerWon) credit else -credit
    }
    (result, playerHand, dealerHand)
  }

  /**
   * Method that prints the player/dealer cards
   */
  def showCards (playerHand: Hand, dealerHand: Hand, maskDealerCards: Boolean = true): Unit = {
    log(s"Dealer hand: [ ${dealerHand.showCards(maskDealerCards)} ]$newLine")
    log(s"Player hand: [ ${playerHand.showCards()} ]$newLine$newLine")
  }

  /**
   * Function that deals the cards to the player & dealer
   */
  def dealCards (deck: Deck): (Hand, Hand) = {
    val playerHand = new Hand
    val dealerHand = new Hand
    // Deal two cards to the player
    playerHand.addCard(deck.dealCard.get)
    playerHand.addCard(deck.dealCard.get)

    // Deal two cards to the dealer
    dealerHand.addCard(deck.dealCard.get)
    dealerHand.addCard(deck.dealCard.get)

    // Return the hands
    (playerHand, dealerHand)
  }

  /**
   * Function that plays out the hand till a winner emerges
   * Return true if player won, false if dealer won
   */
  def didPlayerWin(deck: Deck, playerHand: Hand, dealerHand: Hand): Boolean = {
    var noWinner = true
    var playerWon = false
    val responses = List("S", "H", "h", "s")
    while (noWinner) {
      showCards(playerHand, dealerHand)
      var input = ""
      // Loop till user enters either "S" or "H"
      while (!responses.contains(input)) {
        input = readLine("Hit or Stand? Enter one of (H,h,S,s): ")
      }
      if ("S".equalsIgnoreCase(input)) {
        noWinner = false
        // Deal card to the dealer till his hand's value is over 17
        while (dealerHand.value() < 17) {
          dealerHand.addCard(deck.dealCard.get)
        }
        // Player wins if dealer had goes bust or dealer's hand value is less than that of the player's
        playerWon = dealerHand.isBust || playerHand.winsOver(dealerHand)
        log(s"*** You ${if(playerWon) "win" else "lose"}! *** $newLine")
        showCards(playerHand, dealerHand, maskDealerCards = false)
      } else {
        // If user chooses to "Hit", deal a card
        // Player loses if his hand value goes bust
        playerHand.addCard(deck.dealCard.get)
        if (playerHand.isBust) {
          log("*** You lose! ***")
          showCards(playerHand, dealerHand, maskDealerCards = false)
          noWinner = false
          playerWon = false
        }
      }
    }
    playerWon
  }

}

/**
 * Companion to start game execution
 */
object BlackJack extends App {

  // Instantiate and start game
  new BlackJack().start()

  /**
   * Method that prints the welcome message and rules describing the game
   */
  def printRules(): Unit = {
    log(
      """Welcome to the Single-user BlackJack game!
        |------------------------------------------
        |The game starts by dealing out two cards each to the player and dealer. A score of 21 wins the game for the player or dealer.
        |In the event of a tie, the dealer wins. The user shall receive 100 coins at the beginning of the game.
        |He can choose to bet any number of available coins at each hand. Once all the coins are exhausted, the game ends.
        |The user can choose to "Hit" by entering a "H" when prompted. A "S" response would be assumed to be a "Stand".
        |Good Luck!!
      """.stripMargin)
  }

}
