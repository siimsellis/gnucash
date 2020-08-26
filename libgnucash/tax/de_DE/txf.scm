;; -*-scheme-*-
;; 
;;  2020-08-08	V 4.1	merged the Umsatzsteuer report code from the 'official' files
;;
;;  2020-08-05	V 4.1  	commented out code at the end of file 
;;			which is not present in V4.1 original
;;		changed some DE DA DU and DP codes to better outline 
;;			the  others
;;  2019-09-11	V 3.10 	Modified from tax-de_DE.scm  by JW  
;;	V 3.10 	added German Einkommenssteuer categories
;;	V 3.10 	integrated Umsatzsteuer tax codes 
;;		from the 'official' de_DE tax files
;;		but not the UST tax report code
;;  2019-09-11	V 3.10 	made my EST Codes unique
;;  2018 and earlier 
;;		created a private German Einkommensteuer (EST) version 
;;		throwing out the Umsatzsteuer (UST) tax codes 
;;			and the related tax report routines
;;		starting from the US Version, 
;;		replacing / reusing all the US tax codes 
;;		but retaining the US tax report routines
;;
;; 2010 and earlier
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; Originally, these were meant to hold the codes for the US tax TXF
;; format. Christian Stimming modified this heavily so that it might become useful for
;; the German Umsatzsteuer-Voranmeldung. 
;; Further modifications by:
;;   Jannick Asmus
;;   J. Alex Aycinena
;;   Frank H. Ellenberger
;;   Andreas Köhler
;;   Rolf Leggewie
;; 
;; Der gesamte Inhalt zu den vier Abschnitten der "Umsatzsteuer-Kategorien" 
;; wurden im Dezember 2010 für GnuCash Vers. 2.4.0 vollständig überarbeitet 
;; und alle Einträge gemäß der "Umsatzsteuer-Voranmeldung 2011" umfassend 
;; berichtigt und komplettiert von FJSW - Franz Stoll
;; 
;; This file holds all the Kennzahlen for the
;; Umsatzsteuer-Voranmeldung and their explanations, which can be
;; assigned to particular accounts via the "Edit -> Tax options"
;; dialog. The report in taxtxf-de_DE.scm then will extract the
;; numbers for these Kennzahlen from the actual accounts for a given
;; time period, and will write it to some XML file as required by
;; e.g. the Winston software
;; http://www.felfri.de/winston/schnittstellen.htm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2019  Einkommensteuer:

;; Tax category codes in the US original always seemed to start with an "N"
;;   we avoid "N" and 
;;
;;   continue to use "U" or "K" for Umsatzsteuer
;;   to continue the codes introduced by FJ Stoll and others 
;;
;; and 
;;   introduce "D" for 'Deutsche Einkommenssteuer'
;;   D codes will be "DXnnn" with 
;;   "DM" for 'Mantelbogen', i.e. the main form used to collect all sub-forms
;;   "DR" for sub-form 'R'   dealing with income from 'Renten' retirement items 
;;   "DG" for sub-form 'GSE' dealing with income/expenses from 'Gewerbebetrieb'
;;   "DV" for sub-form 'V'   dealing with income from 'Vermietung und Verpachtung'
;;   "DK" for sub-form 'K'   dealing with income from 'Kapitalvermögen' capital and investment
;;   "DF" for sub-form 'FW'  dealing with 'Förderung von Wohnungseigentum' 
;;
;;   The following codes are only used to show headings for information in EST
;;
;;   "DE" heading only for 'Erträge' 	(Income)
;;   "DU" heading only for 'Aufwendungen'  	(Expense)
;;   "DA" heading only for 'Aktiva'  	(Assets)
;;   "DP" heading only for 'Passiva'  	(Liabilities u.a.)
;;
;;    Code N000 continues to be used to signify 'report only, no export'
;;
;;   The following codes were apparently only used to show headings for information in UST
;;
;;   "U"
;; 
;;  codes should NOT overlap between UST and EST, or the overlapping items will be 
;;  reported in the other tax report, too.



(use-modules (gnucash app-utils))
(use-modules (srfi srfi-2))

(define txf-tax-entity-types
  (list
;; integrated USt into EST
   (cons 'USt #("USt" "Umsatzsteuer"))
;;-----------------------------------------------------------------
   (cons 'DESt #("ESt" "Einkommensteuer"))
;;-----------------------------------------------------------------
   (cons 'Other #("None" "Keine Steuerberichtsoptionen"))))

(define (gnc:tax-type-txf-get-code-info tax-entity-types type-code index)
  (and-let* ((tax-entity-type (assv-ref tax-entity-types type-code)))
    (vector-ref tax-entity-type index)))

(define (gnc:txf-get-tax-entity-type type-code)
  (gnc:tax-type-txf-get-code-info txf-tax-entity-types type-code 0))

(define (gnc:txf-get-tax-entity-type-description type-code)
  (gnc:tax-type-txf-get-code-info txf-tax-entity-types type-code 1))

(define (gnc:txf-get-tax-entity-type-codes)
  (map car txf-tax-entity-types))

(define (gnc:txf-get-payer-name-source categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 0 tax-entity-type))

(define (gnc:txf-get-form categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 1 tax-entity-type))

(define (gnc:txf-get-description categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 2 tax-entity-type))

(define (gnc:txf-get-format categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 3 tax-entity-type))

(define (gnc:txf-get-multiple categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 4 tax-entity-type))

(define (gnc:txf-get-category-key categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 5 tax-entity-type))

(define (gnc:txf-get-line-data categories code tax-entity-type)
  (and-let* ((sym (string->symbol tax-entity-type))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code))
             ((>= (vector-length category) 7)))
                (gnc:txf-get-code-info categories code 6 tax-entity-type)))
(define (gnc:txf-get-last-year categories code tax-entity-type)
  (and-let* ((sym (string->symbol tax-entity-type))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code))
             ((>= (vector-length category) 8)))
                (gnc:txf-get-code-info categories code 7 tax-entity-type)))

(define (gnc:txf-get-help categories code)
  (or (assv-ref txf-help-strings code)
      "Keine Hilfe verfügbar, da nur Gruppenüberschrift.
Diese Kategorie ohne Nummer ==>> N I C H T   V E R W E N D E N !
USt-Kategorien 2011 für GnuCash Vers. 2.4.0 entwickelt und erstellt von: FJSW
Fehlermeldungen + Dankschreiben an: stoll@bomhardt.de"
))

(define (gnc:txf-get-codes categories tax-entity-type)
  (and-let* ((sym (if (string-null? tax-entity-type)
                                               'Ind
                      (string->symbol tax-entity-type)))
             (tax-entity-codes (assv-ref categories sym)))
    (map car tax-entity-codes)))


(define (gnc:txf-get-code-info categories code index tax-entity-type)
  (and-let* ((sym (if (string-null? tax-entity-type)
                                              'Ind
                      (string->symbol tax-entity-type)))
             (tax-entity-codes (assv-ref categories sym))
             (category (assv-ref tax-entity-codes code)))
    (vector-ref category index)))

(define txf-help-categories
  (list
   (cons 'H000 #(current "help" "Name des aktuellen Kontos wird exportiert." 0 #f ""))
   (cons 'H001 #(current "help" "Hilfekategorie H001 noch leer." 0 #f ""))
   (cons 'H002 #(parent "help" "Name des übergeordneten Kontos wird exportiert." 0 #f ""))
   (cons 'H003 #(not-impl "help" "Noch nicht implementiert, NICHT benutzen!" 0 #f ""))))


;; ################################   ERTRÄGE   ###############################

(define txf-income-categories
 (list
  (cons 'USt
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U00 #(none "" "Lieferungen und sonstige Leistungen (einschließlich unentgeltlicher Wertabgaben)" 0 #f ""))
   (cons 'U10 #(none "" "Steuerfreie Umsätze mit Vorsteuerabzug" 0 #f ""))
   (cons 'U11 #(none "" "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG)" 0 #f ""))
   (cons 'K41 #(none "41" "an Abnehmer MIT USt-IdNr." 2 #f "41"))
   (cons 'K44 #(none "44" "neuer Fahrzeuge an Abnehmer OHNE USt-IdNr." 2 #f "44"))
   (cons 'K49 #(none "49" "neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG)" 2 #f "49"))
   (cons 'K43 #(none "43" "Weitere steuerfreie Umsätze mit Vorsteuerabzug" 2 #f "43"))
   (cons 'U12 #(none "" "Steuerfreie Umsätze ohne Vorsteuerabzug" 0 #f ""))
   (cons 'K48 #(none "48" "Umsätze nach § 4 Nr. 8 bis 28 UStG" 2 #f "48"))

   (cons 'U20 #(none "" "Steuerpflichtige Umsätze (Lieferungen u. sonst. Leistungen einschl. unentgeltlicher Wertabgaben)" 0 #f ""))
   (cons 'K51 #(none "51" "zum Steuersatz von 16 %  (bis incl. 2006)" 2 #f "51"))
   (cons 'K81 #(none "81" "zum Steuersatz von 19 %" 2 #f "81"))
   (cons 'K86 #(none "86" "zum Steuersatz von  7 %" 2 #f "86"))
   (cons 'K35 #(none "35" "zu anderen Steuersätzen (Erlöse)" 2 #f "35"))
   (cons 'K36 #(none "36" "zu anderen Steuersätzen (MWSt)" 1 #f "36"))
   (cons 'U21 #(none "" "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG" 0 #f ""))
   (cons 'K77 #(none "77" "an Abnehmer MIT USt-IdNr." 2 #f "77"))
   (cons 'K76 #(none "76" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Erlöse)" 2 #f "76"))
   (cons 'K80 #(none "80" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (MWSt)" 1 #f "80"))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K98 #(none "98" "zu anderen Steuersätzen (Gebuchte MWSt)" 1 #f "98"))
   (cons 'K96 #(none "96" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Gebuchte MWSt)" 1 #f "96"))

   (cons 'U40 #(none "" "Ergänzende Angaben zu Umsätzen" 0 #f ""))
   (cons 'K42 #(none "42" "Lieferungen des ersten Abnehmers bei innergemeinschaftl. Dreiecksgeschäften (§ 25b Abs. 2 UStG)" 2 #f "42"))
   (cons 'K60 #(none "60" "Steuerpflichtige Umsätze, für die der Leistungsempfänger die Steuer nach § 13b Abs. 5 UStG schuldet" 2 #f "60"))
   (cons 'K21 #(none "21" "Nicht steuerbare sonstige Leistungen gem. § 18b Satz 1 Nr. 2 UStG (Leistungen EU-Land)" 2 #f "21"))
   (cons 'K45 #(none "45" "Übrige nicht steuerbare Umsätze (Leistungsort nicht im Inland)" 2 #f "45"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K47 #(none "47" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Gebuchte MWSt)" 1 #f "47"))
   (cons 'K53 #(none "53" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Gebuchte MWSt)" 1 #f "53"))
   (cons 'K74 #(none "74" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (Gebuchte MWSt)" 1 #f "74"))
   (cons 'K85 #(none "85" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Gebuchte MWSt)" 1 #f "85"))

   (cons 'U70 #(none "" "Andere Steuerbeträge" 0 #f ""))
   (cons 'K65 #(none "65" "Steuer infolge Wechsels der Besteuerungsform sowie Nachsteuer auf versteuerte Anzahlungen u. ä." 1 #f "65"))
   (cons 'K69 #(none "69" "In Rechnungen unrichtig oder unberechtigt ausgewiesene Steuerbeträge (§ 14c UStG)" 1 #f "69"))
   )
  )
  
  (cons 'DESt
;;     (einschl. notwendige Kosten dazu)
;;     tax entity type Vector 
;;     guessed format
;;     (cons 'code #(payer-name-source "form" "description" format multiple category-key [([(YEAR_STARTING "field")]...)] ) )
   (list
;;   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))

    (cons 'DE100 #(none "" "----Erträge----" 0 #f ""))
    (cons 'DE000 #(none "" "Erträge, nur informativ -- Kein Export -- " 0 #f ""))


    (cons 'DM600 #(none "" "----Erträge aus Versicherungen und zu außergewöhnl Lasten ----" 0 #f ""))

    (cons 'DM682 #(none "EStMB" "Erstattete Krankenvers." 1 #f "" ((2018 "68/2"))))
    (cons 'DM692 #(none "EStMB" " 69/2 Erstattete Unfallvers." 1 #f "" ((2018 "69/2" ))))
    (cons 'DM702 #(none "EStMB" " 70/2 Erstattet Lebensvers.o.VWL" 1 #f "" ((2018 "70/2" ))))
    (cons 'DM712 #(none "EStMB" " 71/2 Erstatt. Haftpflichtvers." 1 #f "" ((2018 "71/2" ))))
    (cons 'DM762 #(none "EStMB" " 76/2 Erstattete   Kirchensteuer" 1 #f "" ((2018 "76/2" ))))
    (cons 'DM116 #(none "EStMB" "116/2 Erstatt. agB m. zumutb B." 1 #f "" ((2018 "116/2" ))))

    (cons 'DR000 #(none "" "---- Renten ----" 0 #f ""))

    (cons 'DR001 #(none "R" "Renten" 1 #f "" ))


    (cons 'DG100 #(none "" "---- Erträge aus Gewerbebetrieb ----" 0 #f ""))

    (cons 'DG101 #(none "GSE" "Betriebseinnahmen Gewerbebetrieb" 1 #f "" ))
    (cons 'DG102 #(none "GSE" "Einkünfte aus Beteiligungen" 1 #f ""))
    (cons 'DG103 #(none "GSE" "Einnahmen aus selbst. Arbeit" 1 #f ""))
    (cons 'DG104 #(none "GSE" "Einkünfte aus bürgerl. Ges." 1 #f ""))

    (cons 'DV000 #(none "" "---- Erträge aus Vermietung und Verpachtung ----" 0 #f ""))

    (cons 'DV020 #(none "V" " 2 Mieteinnahmen" 1 #f "2"))
    (cons 'DV040 #(none "V" " 4 Mieteinnahmen verbillig. über." 1 #f "4"))
    (cons 'DV061 #(none "V" " 6 Mieteinnahmen f.frühere Jahre" 1 #f "6"))
    (cons 'DV062 #(none "V" " 6 Miet-VZ a. Baukostenvorsch." 1 #f "6"))
    (cons 'DV050 #(none "V" " 5 Einnahmen aus Umlagen" 1 #f "5"))
    (cons 'DV071 #(none "V" " 7 Mieteinnahmen von Werbefläche" 1 #f "7"))
    (cons 'DV072 #(none "V" " 7 Mieteinnahmen von Kiosken" 1 #f "7"))
    (cons 'DV073 #(none "V" " 7 Mieteinnahmen von and. Gebäud" 1 #f "7"))
    (cons 'DV074 #(none "V" " 7 Erstattete Umsatzsteuer" 1 #f "7"))
    (cons 'DV131 #(none "V" "13 Öffentl.Zusch.zu Erhalt.aufw" 1 #f ""))
    (cons 'DV132 #(none "V" "13 sonstige Aufwendungszuschüsse" 1 #f ""))
    (cons 'DV133 #(none "V" "13 erhalt. Bausparvertragszinsen" 1 #f ""))
    (cons 'DV134 #(none "V" "13 erhalt. Nutzungsentschädigung" 1 #f ""))
    (cons 'DV211 #(none "V" "21-1 Ant Eink Bauherrn/Erwerberg" 1 #f ""))
    (cons 'DV261 #(none "V" "26-1 Einn Unterverm v. gem Räume" 1 #f ""))
    (cons 'DV271 #(none "V" "27-1 Einnahme a.and. Verm & Verp" 1 #f ""))

    (cons 'DK000 #(none "" "---- Erträge aus Kapital ----" 0 #f ""))

    (cons 'DK041 #(none "KSO" " 4-1  Zinsen u. and. Ertr.-Stpfl." 1 #f "" ))
    (cons 'DK042 #(none "KSO" " 4-2  Zinsen u. and. Ertr.-Ehefr." 1 #f "" ))
    (cons 'DK043 #(none "KSO" " 4-3  Zinsen u. and. Ertr.-Freis." 1 #f "" ))
    (cons 'DK044 #(none "KSO" " 4-4  Zins/and. Ertr.-KapESt/ZASt" 1 #f "" ))
    (cons 'DK051 #(none "KSO" " 5-1  Bausparguthaben-Steuerpfl." 1 #f "" ))
    (cons 'DK052 #(none "KSO" " 5-2  Bausparguthaben-Ehefrau" 1 #f "" ))
    (cons 'DK053 #(none "KSO" " 5-3  Bausparguthaben-Freistell." 1 #f "" ))
    (cons 'DK054 #(none "KSO" " 5-4  Bausparguthaben-KapESt/ZASt" 1 #f "" ))
    (cons 'DK061 #(none "KSO" " 6-1  Festv.Wepap./Inv.an.-Stpfl." 1 #f "" ))
    (cons 'DK062 #(none "KSO" " 6-2  Festv.Wepap./Inv.an.-Ehefr." 1 #f "" ))
    (cons 'DK063 #(none "KSO" " 6-3  Festv.WP/Inv.an.-Freistell." 1 #f "" ))
    (cons 'DK064 #(none "KSO" " 6-4  Festv.WP/Inv.an.-KESt/ZASt" 1 #f "" ))
    (cons 'DK065 #(none "KSO" " 6-5  Festv.WP/Investm.ant.-KSt" 1 #f "" ))
    (cons 'DK071 #(none "KSO" " 7-1  Tafelg.m.festv.WP-Stpfl." 1 #f "" ))
    (cons 'DK072 #(none "KSO" " 7-2  Tafelg.m.festv.WP-Ehefrau" 1 #f "" ))
    (cons 'DK073 #(none "KSO" " 7-3  Tafelg.m.festv.WP-KESt/ZASt" 1 #f "" ))
    (cons 'DK081 #(none "KSO" " 8-1  And.Kapitalford.-Steuerpfl." 1 #f "" ))
    (cons 'DK082 #(none "KSO" " 8-2  And.Kapitalford.-Ehefrau" 1 #f "" ))
    (cons 'DK083 #(none "KSO" " 8-3  And.Kapitalford.-Freistell." 1 #f "" ))
    (cons 'DK084 #(none "KSO" " 8-4  And.Kapitalford.-KESt/ZASt" 1 #f "" ))
    (cons 'DK091 #(none "KSO" " 9-1  Dividenden a. Aktien-Stpfl." 1 #f "" ))
    (cons 'DK092 #(none "KSO" " 9-2  Dividenden a. Aktien-Ehefr." 1 #f "" ))
    (cons 'DK093 #(none "KSO" " 9-3  Dividenden a. Aktien-Freis." 1 #f "" ))
    (cons 'DK094 #(none "KSO" " 9-4  Divid. a. Aktien-KESt/ZASt" 1 #f "" ))
    (cons 'DK095 #(none "KSO" " 9-5  Dividenden aus Aktien-KSt" 1 #f "" ))
    (cons 'DK101 #(none "KSO" "10-1  Wandelanl./Gewinnobl.-Stpfl" 1 #f "" ))
    (cons 'DK102 #(none "KSO" "10-2  Wandelanl./Gewinnobl.-Ehefr" 1 #f "" ))
    (cons 'DK103 #(none "KSO" "10-3  Wandelanl./Gewinnob.-Freis." 1 #f "" ))
    (cons 'DK104 #(none "KSO" "10-4  Wandelan/Gewinnob-KESt/ZASt" 1 #f "" ))
    (cons 'DK111 #(none "KSO" "11-1  St.pfl.Einn.Lebensv.-Stpfl." 1 #f "" ))
    (cons 'DK112 #(none "KSO" "11-2  Stpfl.Einn.Lebensv.-Ehefrau" 1 #f "" ))
    (cons 'DK113 #(none "KSO" "11-3  Stpfl.Einn.Lebensv.-Freist." 1 #f "" ))
    (cons 'DK114 #(none "KSO" "11-4  Stpfl.Ein.Lebenv.-KESt/ZAS" 1 #f "" ))
    (cons 'DK121 #(none "KSO" "12-1  St.Gesells./part.Darl-Stpfl" 1 #f "" ))
    (cons 'DK122 #(none "KSO" "12-2  St.Gesells./part.Darl-Ehefr" 1 #f "" ))
    (cons 'DK123 #(none "KSO" "12-3  St.Gesells./part.Darl-Freis" 1 #f "" ))
    (cons 'DK124 #(none "KSO" "12-4  St.Ges./part.Darl.KESt/ZASt" 1 #f "" ))
    (cons 'DK142 #(none "KSO" "14-2  Beteiligungen-Steuerpfl." 1 #f "" ))
    (cons 'DK143 #(none "KSO" "14-3  Beteiligungen-Ehefrau" 1 #f "" ))
    (cons 'DK144 #(none "KSO" "14-4  Beteiligungen-KapESt/ZASt" 1 #f "" ))
    (cons 'DK145 #(none "KSO" "14-5  Beteiligungen-KSt" 1 #f "" ))
    (cons 'DK151 #(none "KSO" "15-1  Kap.ford.ohne St.abz.-Stpfl" 1 #f "" ))
    (cons 'DK152 #(none "KSO" "15-2  Kap.ford.o.Steuerab.-Ehefr" 1 #f "" ))
    (cons 'DK161 #(none "KSO" "16-1  Erst.zins Finanzamt-Stpfl." 1 #f "" ))
    (cons 'DK162 #(none "KSO" "16-2  Erst.zins Finanzamt-Ehefr." 1 #f "" ))
    (cons 'DK201 #(none "KSO" "20-1  Ausländ.Kapitalertr.-Stpfl" 1 #f "" ))
    (cons 'DK202 #(none "KSO" "20-2  Ausl.Kapitalerträge-Ehefr." 1 #f "" ))
    (cons 'DK203 #(none "KSO" "20-3  Ausl.Kapitalerträge-Freis." 1 #f "" ))
    (cons 'DK460 #(none "KSO" "46-46 Unterhaltsleistungen" 1 #f "" ))

   )
  )
  (cons 'Other
   (list
    (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
 )
)

;; ################################   AUFWENDUNGEN   ###############################
(define txf-expense-categories
 (list
;; tax entity type Vector 
;;   (payer-name-source form description format multiple category-key)
;; tentative assignments:
;;   #1 Payer-name-source: unknown
;;   #2 Form name, i.e. the paper form this item's content would go to
;;   #3 Description: the human readable brief explanation of the item
;;   #4 Format: unknown
;;   #5 multiple categorie-key: unknown
;;
  (cons 'USt
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U31 #(none "" "Steuerfreie innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K91 #(none "91" "Erwerbe nach § 4b UStG" 2 #f "91"))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K89 #(none "89" "zum Steuersatz von 19 %" 2 #f "89"))
   (cons 'K93 #(none "93" "zum Steuersatz von  7 %" 2 #f "93"))
   (cons 'K95 #(none "95" "zu anderen Steuersätzen (Aufwand)" 2 #f "95"))
   (cons 'K94 #(none "94" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Aufwand)" 2 #f "94"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K46 #(none "46" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Aufwand)" 2 #f "46"))
   (cons 'K52 #(none "52" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Aufwand)" 2 #f "52"))
   (cons 'K73 #(none "73" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze die unter das GrEStG fallen (Aufwand)" 2 #f "73"))
   (cons 'K84 #(none "84" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Aufwand)" 2 #f "84"))

   (cons 'U60 #(none "" "Abziehbare Vorsteuerbeträge" 0 #f ""))
   (cons 'K66 #(none "66" "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG)" 1 #f "66"))
   (cons 'K61 #(none "61" "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen (Gebuchte VSt)" 1 #f "61"))
   (cons 'K62 #(none "62" "Entrichtete Einfuhrumsatzsteuer (§ 15 Abs. 1 Satz 1 Nr. 2 UStG)" 1 #f "62"))
   (cons 'K67 #(none "67" "Vorsteuerbeträge aus Leistungen im Sinne des § 13b UStG (§ 15 Abs. 1 Satz 1 Nr. 4 UStG) (Gebuchte VSt)" 1 #f "67"))
   (cons 'K63 #(none "63" "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind (§§ 23 und 23a UStG)" 1 #f "63"))
   (cons 'K64 #(none "64" "Berichtigung des Vorsteuerabzugs (§ 15a UStG)" 1 #f "64"))
   (cons 'K59 #(none "59" "Vorsteuerabzug für innergemeinschaftl. Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 1 #f "59"))

   (cons 'U70 #(none "" "Andere Steuerbeträge" 0 #f ""))
   (cons 'K39 #(none "39" "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung (1/11) für Dauerfristverlängerung" 1 #f "39"))

   )
  )
  (cons 'DESt
   (list
    (cons 'DU000 #(none "" "-----Aufwendungen----" 0 #f ""))
    (cons 'DU001 #(none "" "Aufwendung nur Report -- Kein Export -- " 0 #f ""))

    (cons 'DU002 #(none "" "--- Versicherungen und außergewöhnl Lasten --" 0 #f ""))

    (cons 'DM670 #(none "EStMB" " 67   Freiw.Beiträge zur Rentenv." 1 #f "" ))
    (cons 'DM681 #(none "EStMB" " 68/1 Gezahlte Krankenvers." 1 #f "" ))
    (cons 'DM691 #(none "EStMB" " 69/1 Gezahlte Unfallvers." 1 #f "" ))
    (cons 'DM701 #(none "EStMB" " 70/1 Gezahlte Lebensvers.o.VWL" 1 #f "" ))
    (cons 'DM711 #(none "EStMB" " 71/1 Gezahlte Haftpflichtvers." 1 #f "" ))

    (cons 'DM733 #(none "EStMB" " 73/3 Gezahlte Rente" 1 #f "" ))
    (cons 'DM743 #(none "EStMB" " 74/3 Dauernde Last" 1 #f "" ))
    (cons 'DM750 #(none "EStMB" " 75   Unterhaltsleistung" 1 #f "" ))

    (cons 'DM761 #(none "EStMB" " 76/1 Nachgezahlte Kirchensteuer" 1 #f "" ))
    (cons 'DM770 #(none "EStMB" " 77   Zins f. Nachfordg/Stundung" 1 #f "" ))

    (cons 'DM790 #(none "EStMB" " 79   Hauswirtschaf. Besch.Verh." 1 #f "" ))

    (cons 'DM800 #(none "EStMB" " 80   Steuerberatungskosten" 1 #f "" ))

    (cons 'DM820 #(none "EStMB" " 82   Eigene Berufsausbildung" 1 #f "" ))
    (cons 'DM830 #(none "EStMB" " 83   Schulgeld" 1 #f "" ))

    (cons 'DM850 #(none "EStMB" " 85   Spende: wiss., mildt., kult." 1 #f "" ))
    (cons 'DM860 #(none "EStMB" " 86   Spende: kirchl, relig, gem." 1 #f "" ))
    (cons 'DM870 #(none "EStMB" " 87   Spende an Partei" 1 #f "" ))
    (cons 'DM880 #(none "EStMB" " 88   Spende an unab.Wählerverei" 1 #f "" ))

    (cons 'DM916 #(none "EStMB" "116/1 Außergew Bel. m.zumutb B." 1 #f "" ))


    (cons 'DN000 #(none "" "-- Werbungskosten nichtselbst Arbeit --" 0 #f ""))

    (cons 'DN380 #(none "N" "38 Öffentliche Verkehrsmittel" 1 #f "" ))
    (cons 'DN400 #(none "N" "40 Beitrag Berufsverband" 1 #f "" ))
    (cons 'DN441 #(none "N" "44 Arbeitsmittel / Computerbedarf" 1 #f "" ))
    (cons 'DN442 #(none "N" "44 Arbeitsmittel / Fachliteratur" 1 #f "" ))
    (cons 'DN443 #(none "N" "44 Arbeitsmittel / Büromaterial" 1 #f "" ))
    (cons 'DN444 #(none "N" "44 Arbeitsmittel / Berufsversich." 1 #f "" ))
    (cons 'DN445 #(none "N" "44 Arbeitsmittel / Berufskleidung" 1 #f "" ))
    (cons 'DN446 #(none "N" "44 Arbeitsmittel / Fortbildungsk." 1 #f "" ))
    (cons 'DN447 #(none "N" "44 Arbeitsmittel / GWG" 1 #f "" ))
    (cons 'DN448 #(none "N" "44 Arbeitsmittel / Kundengeschenk" 1 #f "" ))
    (cons 'DN449 #(none "N" "44 Weitere Werbungskosten" 1 #f "" ))
    (cons 'DN440 #(none "N" "44 Arbeitsmittel / Arbeitszimmer" 1 #f "" ))
    
(cons 'DG200 #(none "" "-- Betriebsausgaben --" 0 #f ""))
    
    (cons 'DG201 #(none "GSE" "Betriebsausgaben Gewerbebetr." 1 #f ""))
    (cons 'DG202 #(none "GSE" "Betriebsausg. selbst. Arbeit" 1 #f ""))

(cons 'DK000 #(none "" "-- Werbungskosten KSO / KAP --" 0 #f ""))

    (cons 'DK251 #(none "KSO" "25-1  Werbungskosten-Beteiligung" 1 #f "" ))
    (cons 'DK252 #(none "KSO" "25-2  Werbungskosten-Steuerpfl." 1 #f "" ))
    (cons 'DK253 #(none "KSO" "25-3  Werbungskosten-Ehefrau" 1 #f "" ))
    
    (cons 'DK301 #(none "KAP" "abgeführte Kapitalertragssteuer lt Beschgg" 1 #f ""))
    (cons 'DK302 #(none "KAP" "abgeführter Soli lt Beschgg" 1 #f ""))
    (cons 'DK303 #(none "KAP" "abgeführte Kirchenst auf KapErtrSt lt Beschgg" 1 #f ""))

(cons 'DV000 #(none "" "-- Werbungskosten Vermietung & Verpachtung --" 0 #f ""))

    (cons 'DV341 #(none "V" "34 Schuldzinsen" 1 #f ""))
    (cons 'DV342 #(none "V" "34 Geldbeschaffungskosten" 1 #f ""))
    (cons 'DV440 #(none "V" "44 Renten" 1 #f ""))
    (cons 'DV450 #(none "V" "45 dauernde Lasten" 1 #f ""))
    (cons 'DV511 #(none "V" "51 Grundsteuer" 1 #f ""))
    (cons 'DV512 #(none "V" "51 Straßenreinigung" 1 #f ""))
    (cons 'DV513 #(none "V" "51 Müllabfuhr" 1 #f ""))
    (cons 'DV521 #(none "V" "52 Wasserversorgung" 1 #f ""))
    (cons 'DV522 #(none "V" "52 Entwässerung" 1 #f ""))
    (cons 'DV523 #(none "V" "52 Hausbeleuchtung" 1 #f ""))
    (cons 'DV531 #(none "V" "53 Heizung" 1 #f ""))
    (cons 'DV532 #(none "V" "53 Warmwasser" 1 #f ""))
    (cons 'DV541 #(none "V" "54 Schornsteinreingung" 1 #f ""))
    (cons 'DV542 #(none "V" "54 Hausversicherungen" 1 #f ""))
    (cons 'DV551 #(none "V" "55 Hauswart" 1 #f ""))
    (cons 'DV552 #(none "V" "55 Treppenreinigung" 1 #f ""))
    (cons 'DV553 #(none "V" "55 Fahrstuhl" 1 #f ""))
    (cons 'DV560 #(none "V" "56 Sonstiges" 1 #f ""))
    (cons 'DV662 #(none "V" "26-2 Werbungsk Unterverm gem Räu" 1 #f ""))



   )
  )
  (cons 'Other
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
 )
)

;; #################################    Aktiva     #################################
(define txf-asset-categories
 (list
;; tax entity type Vector (payer-name-source form description format multiple category-key)

  (cons 'USt
   (list
  (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U31 #(none "" "Steuerfreie innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K91 #(none "91" "Erwerbe nach § 4b UStG" 2 #f "91"))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K89 #(none "89" "zum Steuersatz von 19 %" 2 #f "89"))
   (cons 'K93 #(none "93" "zum Steuersatz von  7 %" 2 #f "93"))
   (cons 'K95 #(none "95" "zu anderen Steuersätzen" 2 #f "95"))
   (cons 'K94 #(none "94" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Aufwand)" 2 #f "94"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K46 #(none "46" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Aufwand)" 2 #f "46"))
   (cons 'K52 #(none "52" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Aufwand)" 2 #f "52"))
   (cons 'K73 #(none "73" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze die unter das GrEStG fallen (Aufwand)" 2 #f "73"))
   (cons 'K84 #(none "84" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Aufwand)" 2 #f "84"))

   (cons 'U60 #(none "" "Abziehbare Vorsteuerbeträge" 0 #f ""))
   (cons 'K66 #(none "66" "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG)" 1 #f "66"))
   (cons 'K61 #(none "61" "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen (Gebuchte VSt)" 1 #f "61"))
   (cons 'K62 #(none "62" "Entrichtete Einfuhrumsatzsteuer (§ 15 Abs. 1 Satz 1 Nr. 2 UStG)" 1 #f "62"))
   (cons 'K67 #(none "67" "Vorsteuerbeträge aus Leistungen im Sinne des § 13b UStG (§ 15 Abs. 1 Satz 1 Nr. 4 UStG) (Gebuchte VSt)" 1 #f "67"))
   (cons 'K63 #(none "63" "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind (§§ 23 und 23a UStG)" 1 #f "63"))
   (cons 'K64 #(none "64" "Berichtigung des Vorsteuerabzugs (§ 15a UStG)" 1 #f "64"))
   (cons 'K59 #(none "59" "Vorsteuerabzug für innergemeinschaftl. Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 1 #f "59"))

   )
  )
  (cons 'DESt
   (list
    (cons 'DA000 #(none "" "--     Aktiva    --" 0 #f ""))

    (cons 'DF300 #(none "" "-- Wohneigentum --" 0 #f ""))

    (cons 'DF361 #(none "FW" " 36/1 Anschaffungskosten Grd/Bod" 1 #f "" ))
    (cons 'DF363 #(none "FW" " 36/3 Anschaffungskosten Gebäude" 1 #f "" ))
    (cons 'DF364 #(none "FW" " 36/3 ANK Gebäude Gebühren&Honra" 1 #f "" ))
    (cons 'DF365 #(none "FW" " 36/3 Herstellkosten Gebäude" 1 #f "" ))
    (cons 'DF521 #(none "FW" " 52-1 Schuldzinsen vor Bezug" 1 #f "" ))
    (cons 'DF522 #(none "FW" " 52-2 Disagio vor Bezug" 1 #f "" ))
    (cons 'DF523 #(none "FW" " 52-3 Geldbeschaffungsk. vor Bezug" 1 #f "" ))
    (cons 'DF531 #(none "FW" " 53   Erhaltungsaufwand vor Bezug" 1 #f "" ))
    (cons 'DF532 #(none "FW" " 53   Andere Aufwendung vor Bezug" 1 #f "" ))
   )
  )
  (cons 'Other
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
 )
)

;; ################################   PASSIVA     ###############################

(define txf-liab-eq-categories
 (list
  (cons 'USt
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U20 #(none "" "Steuerpflichtige Umsätze (Lieferungen u. sonst. Leistungen einschl. unentgeltlicher Wertabgaben)" 0 #f ""))
   (cons 'K36 #(none "36" "zu anderen Steuersätzen (MWSt)" 1 #f "36"))
   (cons 'U21 #(none "" "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG" 0 #f ""))
   (cons 'K80 #(none "80" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (MWSt)" 1 #f "80"))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K98 #(none "98" "zu anderen Steuersätzen (Gebuchte MWSt)" 1 #f "98"))
   (cons 'K96 #(none "96" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Gebuchte MWSt)" 1 #f "96"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K47 #(none "47" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Gebuchte MWSt)" 1 #f "47"))
   (cons 'K53 #(none "53" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Gebuchte MWSt)" 1 #f "53"))
   (cons 'K74 #(none "74" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (Gebuchte MWSt)" 1 #f "74"))
   (cons 'K85 #(none "85" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Gebuchte MWSt)" 1 #f "85"))

   (cons 'U70 #(none "" "Andere Steuerbeträge" 0 #f ""))
   (cons 'K65 #(none "65" "Steuer infolge Wechsels der Besteuerungsform sowie Nachsteuer auf versteuerte Anzahlungen u. ä." 1 #f "65"))
   (cons 'K69 #(none "69" "In Rechnungen unrichtig oder unberechtigt ausgewiesene Steuerbeträge (§ 14c UStG)" 1 #f "69"))
   (cons 'K39 #(none "39" "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung (1/11) für Dauerfristverlängerung" 1 #f "39"))
   )
  )
  (cons 'DESt
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
;;   (cons 'N000 #(none "" "Tax Report Only - No TXF Export" 0 #f ""))
    
   )
  )
  (cons 'Other
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
)
)
