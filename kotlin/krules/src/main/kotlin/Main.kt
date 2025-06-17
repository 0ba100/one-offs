package org.cabbage.krules

fun main() {

}

data class Response (val request: Request) {
    var denies: List<Decision> = listOf()
    var allows: List<Decision> = listOf()
    var indeterminate: List<Decision> = listOf()

    fun include(decision: Decision) {
        when (decision.effect) {
            Effect.ALLOW -> allows += decision
            Effect.DENY -> denies += decision
            Effect.INDETERMINATE -> indeterminate += decision
        }
    }

    fun finalDecision(): Decision {
        return when {
            denies.isNotEmpty() -> Decision(Effect.DENY, Reason.from(denies.joinToString(separator = "; ") { it.reason.message }))
            allows.isNotEmpty() -> Decision(Effect.ALLOW, Reason.from("No denies found"))
            else -> Decision(Effect.DENY, Reason.from("No allows found"))
        }
    }
}

fun List<Decision>.aggMessage(): String {
    return this.joinToString(separator = "; ") { it.reason.message }
}

data class Decision (val effect: Effect, val reason: Reason) {
}

enum class Effect {
    ALLOW, DENY, INDETERMINATE
}

data class Request(val user: User, val action: Action, val resource: Resource)
data class User(val name: String, val identifier: String)
data class Action(val name: String, val identifier: String)
data class Resource(val name: String, val identifier: String)

data class Reason(val message: String) {
    companion object {
        fun from(message: String): Reason {
            return Reason(message)
        }
    }
}
