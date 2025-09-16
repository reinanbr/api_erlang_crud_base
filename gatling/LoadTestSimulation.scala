import io.gatling.core.Predef._
import io.gatling.http.Predef._
import scala.concurrent.duration._

class LoadTestSimulation extends Simulation {

  // HTTP configuration
  val httpProtocol = http
    .baseUrl("http://localhost:8081")
    .acceptHeader("application/json")
    .contentTypeHeader("application/json")
    .userAgentHeader("Gatling Load Test")

  // Feeder for generating unique usernames and passwords
  val userFeeder = (1 to 10000).map { i =>
    Map(
      "username" -> s"user$i",
      "password" -> s"password$i"
    )
  }.circular

  // Scenario for user registration
  val registerScenario = scenario("User Registration Load Test")
    .feed(userFeeder)
    .exec(
      http("Register User")
        .post("/register")
        .body(StringBody("""{"username": "${username}", "password": "${password}"}"""))
        .check(status.in(200, 201, 409)) // Accept success and conflict (duplicate user)
        .check(jsonPath("$.success").optional.saveAs("registrationSuccess"))
    )
    .exec { session =>
      val success = session("registrationSuccess").asOption[String]
      success match {
        case Some("true") => 
          println(s"✓ Successfully registered user: ${session("username").as[String]}")
        case Some("false") =>
          println(s"⚠ Failed to register user: ${session("username").as[String]}")
        case _ =>
          println(s"? Unknown response for user: ${session("username").as[String]}")
      }
      session
    }

  // Scenario for testing authenticated endpoints (login + balance check)
  val authScenario = scenario("Authentication and Balance Check")
    .feed(userFeeder)
    .exec(
      http("Login")
        .post("/login")
        .body(StringBody("""{"username": "${username}", "password": "${password}"}"""))
        .check(status.in(200, 401))
        .check(jsonPath("$.token").optional.saveAs("authToken"))
        .check(jsonPath("$.success").optional.saveAs("loginSuccess"))
    )
    .doIf(session => session("loginSuccess").asOption[String].contains("true")) {
      exec(
        http("Get Balance")
          .get("/balance")
          .header("Authorization", "Bearer ${authToken}")
          .check(status.is(200))
          .check(jsonPath("$.balance").saveAs("userBalance"))
      )
      .exec { session =>
        val balance = session("userBalance").asOption[String]
        println(s"✓ User ${session("username").as[String]} balance: $balance")
        session
      }
    }

  // Scenario for transaction testing
  val transactionScenario = scenario("Transaction Load Test")
    .feed(userFeeder)
    .exec(
      http("Login for Transaction")
        .post("/login")
        .body(StringBody("""{"username": "${username}", "password": "${password}"}"""))
        .check(status.is(200))
        .check(jsonPath("$.token").saveAs("authToken"))
    )
    .pause(100.milliseconds)
    .exec(
      http("Buy Transaction")
        .post("/buy")
        .header("Authorization", "Bearer ${authToken}")
        .body(StringBody("""{"amount": 10.50}"""))
        .check(status.in(200, 400)) // Accept success or insufficient funds
    )
    .pause(100.milliseconds)
    .exec(
      http("Sell Transaction")
        .post("/sell")
        .header("Authorization", "Bearer ${authToken}")
        .body(StringBody("""{"amount": 5.25}"""))
        .check(status.is(200))
    )

  // Load injection patterns
  setUp(
    // Primary test: 10,000 registrations in 30 seconds
    registerScenario.inject(
      rampUsers(10000).during(30.seconds)
    ).protocols(httpProtocol),
    
    // Secondary test: Authentication load
    authScenario.inject(
      nothingFor(35.seconds), // Wait for registration to complete
      rampUsers(1000).during(10.seconds)
    ).protocols(httpProtocol),
    
    // Tertiary test: Transaction load
    transactionScenario.inject(
      nothingFor(50.seconds), // Wait for previous tests
      rampUsers(500).during(15.seconds)
    ).protocols(httpProtocol)
  ).assertions(
    // Assertions for registration scenario
    forAll.failedRequests.percent.lte(5), // Less than 5% failures
    forAll.responseTime.percentile3.lte(2000), // 95% of requests under 2s
    global.responseTime.mean.lte(500), // Mean response time under 500ms
    
    // Specific assertions for registration endpoint
    details("Register User").responseTime.percentile3.lte(1000),
    details("Register User").successfulRequests.percent.gte(95)
  )
}
