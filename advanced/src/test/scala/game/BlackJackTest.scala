package game

import org.scalatest.{FlatSpec, Matchers}

class BlackJackTest extends FlatSpec with Matchers{

  behavior of "Hand"

  it should "start with no cards" in {
    val hand = new Hand
    hand.cards.size should equal(0)
  }

  behavior of "Deck"

  it should "start with shuffled deck of 52 cards" in {
    val deck = new Deck
    deck.cards.size should equal(52)
  }

  behavior of "BlackJack"

  it should "deal cards" in {
    val blackJack = new BlackJack()
    val deck = new Deck
    deck.cards.clear()
    deck.cards += Card(Heart, King)
    deck.cards += Card(Spade, Ten)
    deck.cards += Card(Heart, Two)
    deck.cards += Card(Spade, Four)
    val (playerHand, dealerHand) = blackJack.dealCards(deck)
    playerHand.cards.head should equal (Card(Heart, King))
    playerHand.cards.tail.head should equal (Card(Spade, Ten))
    dealerHand.cards.head should equal (Card(Heart, Two))
    dealerHand.cards.tail.head should equal (Card(Spade, Four))
  }

  it should "player should win if player gets blackjack" in {
    val blackJack = new BlackJack()
    val deck = new Deck
    deck.cards.clear()
    deck.cards += Card(Heart, King)
    deck.cards += Card(Spade, Ace)
    deck.cards += Card(Heart, Two)
    deck.cards += Card(Spade, Four)
    val (result, playerHand, dealerHand) = blackJack.playHand(100, deck)
    result should equal (100)
    playerHand.cards.head should equal (Card(Heart, King))
    playerHand.cards.tail.head should equal (Card(Spade, Ace))
    dealerHand.cards.head should equal (Card(Heart, Two))
    dealerHand.cards.tail.head should equal (Card(Spade, Four))
  }
}
