module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Parte 1" $ do
        describe "Precio final Hamburguesa" $ do
            it "El precio final de una hamburguesa es el precio base + suma de los precios de los ingredientes" $ do
                precioFinal cuartoDeLibra `shouldBe` 54
        
        describe "agregarIngrediente" $ do
            it "Agregarle Curry al cuarto de libra" $ do
                agregarIngrediente cuartoDeLibra Curry `shouldBe` (Hamburguesa 20 [Curry, Pan, Carne, Cheddar, Pan])
        describe "agrandar" $ do
            it "Agrandar el cuarto de libra" $ do
                agrandar cuartoDeLibra `shouldBe` (Hamburguesa 20 [Carne, Pan, Carne, Cheddar, Pan])
        describe "descuento" $ do
            it "Aplica un 20% de descuento al cuarto de libra" $ do
                descuento 20 cuartoDeLibra `shouldBe` (Hamburguesa 16 [Pan, Carne, Cheddar, Pan])
        describe "pdepBurger" $ do
            it "es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento" $ do
                pdpBurguer `shouldBe` (Hamburguesa 16 [ Carne, Carne, Cheddar, Panceta, Pan, Carne, Cheddar, Pan])
        describe "precioFinal de pdepBurger" $ do
            it "El precio final de la pdpBurguer" $ do
                precioFinal pdpBurguer `shouldBe` 110

    describe "Parte 2" $ do
        describe "dobleCuarto" $ do
            it "Es un cuarto de libra con carne y cheddar" $ do
                dobleCuarto `shouldBe` (Hamburguesa 20 [Carne, Cheddar, Pan, Carne, Cheddar, Pan])
            it "El precio final del doble cuarto de libra" $ do
                precioFinal dobleCuarto `shouldBe` 84
        describe "bigPdp" $ do
            it "Es un doble cuarto con Curry" $ do
                bigPdp `shouldBe` (Hamburguesa 20 [Curry, Carne, Cheddar, Pan, Carne, Cheddar, Pan])
            it "El precio final de bigPdp" $ do
                precioFinal bigPdp `shouldBe` 89
        describe "delDia" $ do
            it "Big Pdep del dia" $ do
                delDia bigPdp `shouldBe` (Hamburguesa 14 [Papas, Curry, Carne, Cheddar, Pan, Carne, Cheddar, Pan])
            it "El precio final de doble cuarto del dia" $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88

    describe "Parte 3" $ do
        describe "hacerVeggie" $ do
            it "hacer veggie a la pdpBurguer" $ do
                hacerVeggie pdpBurguer `shouldBe` (Hamburguesa 16 [PatiVegano, PatiVegano, QuesoDeAlmendras, BaconTofu, Pan, PatiVegano, QuesoDeAlmendras, Pan])
            it "el precio final de pdpBurguer veggie" $ do
                precioFinal (hacerVeggie pdpBurguer) `shouldBe` 90
        describe "cambiarPanDePati" $ do
            it "cambiar pan de cuarto de libra" $ do
                cambiarPanDePati cuartoDeLibra `shouldBe` (Hamburguesa 20 [PanIntegral, Carne, Cheddar, PanIntegral])
            it "el precio final de cuarto de libra con pan integral" $ do
                precioFinal (cambiarPanDePati cuartoDeLibra) `shouldBe` 56
        describe "dobleCuartoVegano" $ do
            it "doble cuarto veggie" $ do
                dobleCuartoVegano `shouldBe` (Hamburguesa 20 [PatiVegano, QuesoDeAlmendras, PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral])
            it "el precio final del doble cuarto veggie" $ do
                precioFinal dobleCuartoVegano `shouldBe` 76
