# acv-engine

**acv-engine** est un moteur d'évaluation du cycle de vie (ACV) minimaliste, écrit en Haskell, fonctionnant entièrement en mémoire vive. Il permet de :

- Charger des procédés Ecoinvent au format `.spold` (EcoSpold XML)
- Construire un arbre de procédés dépendants
- Calculer l'inventaire environnemental (LCI)
- Appliquer une méthode de caractérisation (ex : PEF)
- Exporter les résultats en format ILCD (XML) ou CSV

---

## Fonctionnalités

- [x] Lecture de procédés `.spold` (Ecoinvent, EcoSpold 1 ou 2)
- [x] Méthode de caractérisation au format ILCD XML (ex: PEF)
- [x] Calcul récursif de l'inventaire par parcours d'arbre
- [x] Application de facteurs de caractérisation
- [x] Export de l'inventaire en :
  - XML (format ILCD simplifié)
  - CSV lisible

---

## Installation

### Prérequis

- GHC >= 8.10
- [Cabal](https://www.haskell.org/cabal/)
- `make` (optionnel)

### Compilation

```bash
cabal build
```

### Exécution

```bash
cabal run acv-cli -- \
  --data chemin/vers/spolds \
  --root uuid-du-procédé \
  --method chemin/vers/methode.xml \
  [--output resultat.xml] \
  [--csv resultat.csv]
```

---

## Exemple

Supposons que vous ayez :

- Un procédé `ble.spold` avec l'UUID `"uuid-blé"`
- Une méthode `PEF.xml`

Alors :

```bash
cabal run acv-cli -- \
  --data ./acv-engine-examples \
  --root uuid-blé \
  --method ./acv-engine-examples/PEF.xml \
  --output inventaire.xml \
  --csv inventaire.csv
```

---

## Structure du projet

```
src/
├── ACV/
│   ├── Types.hs         -- Types de base : Process, Flow, Exchange, etc.
│   ├── Tree.hs          -- Construction de l'arbre ACV
│   ├── Inventory.hs     -- Calcul de l'inventaire
│   ├── PEF.hs           -- Caractérisation LCIA
│   └── Export/
│       ├── CSV.hs       -- Export CSV
│       └── ILCD.hs      -- Export XML ILCD
├── EcoSpold/
│   ├── Parser.hs        -- Lecture de fichiers .spold
│   └── Loader.hs        -- Chargement de tous les procédés d’un dossier
└── ILCD/
    └── Parser.hs        -- Lecture d’une méthode PEF (ILCD XML)
```

---

## Tests

```bash
cabal test
```

Un test est inclus dans `test/Spec.hs` et valide le score PEF attendu d’un exemple de blé dur.

---

## Licence

MIT © Toi

---

## À venir

- Support des incertitudes (distributions, Monte Carlo)
- Support de Brightway2 ou ecoinvent JSON-LD
- Interface graphique ou web (via `servant` ou `reflex`)

---

## Auteur

Projet conçu pour la recherche, la pédagogie ou les démonstrateurs ACV.
