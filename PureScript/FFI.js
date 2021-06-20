"use strict"

exports.calculateInterest = function (amount) {
  return amount * 0.1
}

exports.calculateInterest2 = function (amount, months) {
  return amount * Math.exp(0.1, months)
}

exports.calculateInterest3 = function (amount) {
  return function (months) {
    return amount * Math.exp(0.1, months)
  }
}
