package org.multibot

import org.scalatest.{Assertion, FlatSpec}

class InputSanitizerTest extends FlatSpec {

  def ensureSanitizedInput(in: String, expected: String): Assertion =
    assert(GitterInputSanitizer.sanitize(in) === expected)

  def ensureNoSanitization(in: String) : Assertion =
    ensureSanitizedInput(in, in)

  "Inputs" should "be sanitized" in {
    ensureSanitizedInput("```\nfoo\n```", "foo\n")
    ensureSanitizedInput("`foo`", "foo")

    // I have no idea why anyone would do this, but make sure the result is expected
    ensureSanitizedInput("```\n`foo`\n```", "`foo`\n")
    ensureSanitizedInput("``foo``", "`foo`")
  }

  it should "not be sanitized" in {
    ensureNoSanitization("foo")
  }
}
