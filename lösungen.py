from pickle import FALSE


def verdoppeln_quadrieren_helper(zahl:int, verdoppeln:bool) -> int:
    if zahl >= 1000000:
        return zahl
    if verdoppeln:
        zahl = zahl*2
        return verdoppeln_quadrieren_helper(zahl, False)
    else:
        zahl = zahl*zahl
        return verdoppeln_quadrieren_helper(zahl, True)

def verdoppeln_quadrieren(zahl: int) -> int:
    return verdoppeln_quadrieren_helper(zahl, True)

print(verdoppeln_quadrieren(500))

def verdoppeln_quadrieren_alt_helper(zahl:int, verdoppeln:bool, schritt:int) -> int:
    if zahl >= 1000000:
        return schritt
    if verdoppeln:
        zahl = zahl*2
        schritt = schritt + 1
        return verdoppeln_quadrieren_alt_helper(zahl, False, schritt)
    else:
        zahl = zahl*zahl
        schritt = schritt + 1
        return verdoppeln_quadrieren_alt_helper(zahl, True, schritt)

def verdoppeln_quadrieren_alt(zahl:int) -> int:
    return verdoppeln_quadrieren_alt_helper(zahl, True, 0)

print(verdoppeln_quadrieren_alt(500))