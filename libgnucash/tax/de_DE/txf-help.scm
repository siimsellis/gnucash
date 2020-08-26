;; -*-scheme-*-
;;
;;  2020-08-08	V 4.1	merged the Umsatzsteuer report code from the 
;;                            'official' files
;;
;;  2020-08-05	V 4.1  	commented out code at the end of file 
;;		which is not present in V4.1 original
;;		changed some DE DA DU and DP codes to better 
;;                            outline  the  others
;;  2019-09-11	V 3.10 	Modified from tax-de_DE.scm  by JW  
;;	V 3.10 	added German Einkommenssteuer categories
;;	V 3.10 	integrated Umsatzsteuer tax codes 
;;		from the 'official' de_DE tax files
;;		but not the UST tax report code
;;  2019-09-11	V 3.10 	made my EST Codes unique
;;  2018 and earlier 	created a private German Einkommensteuer (EST)
;;		version 
;;		throwing out the Umsatzsteuer (UST) tax codes 
;;		and the related tax report routines
;;		starting from the US Version, 
;;		replacing / reusing all the US tax codes 
;;		but retaining the US tax report routines
;;
;; 2010 and earlier
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; Originally, these were meant to hold the codes for the US tax TXF
;; format. Christian Stimming modified this heavily so that it might become 
;; useful for the German Umsatzsteuer-Voranmeldung. 
;;
;; Diese Datei wurde im Dezember 2010 für GnuCash Vers. 2.4.0 vollständig 
;; überarbeitet und alle Einträge gemäß der "Umsatzsteuer-Voranmeldung 2011"
;; umfassend berichtigt und komplettiert von FJSW - Franz Stoll
;;
;;
;; This file holds the explanations to the categories from txf-de_DE.scm.
;;
;; Changes in this file should also be applied on 
;;	gnucash-docs/help/de/Help_txf-categories.xml
;;
;; Joachim Wetzig modified private versions of this file, beginning 2017, 
;; to create a version which can be used for the German Income Tax 
;; declaration (Einkommensteuer, ESt), initially throwing out the 
;; Umsatzsteuervoranmeldung (USt) specific code
;;
;; Starting 2019  Einkommensteuer and Umsatzsteuer were both included
;; The detailed change history is given at the beginning of this file
;;
;; Tax category codes in the US original always seemed to start with an "N"
;;   avoid "N" and continue to use "U" or "K" for Umsatzsteuer
;;   to continue the codes introduced by FJ Stoll and others 
;; and 
;;   introduce "D" for 'Deutsche Einkommenssteuer'
;;   Our codes will be "DXnnn" with 
;;   "DM" for 'Mantelbogen', 	i.e. the main paper form used to collect all 
;;		sub-forms
;;   "DR" for sub-form 'R'   	dealing with income from 'Renten' 
;;		retirement items 
;;   "DG" for sub-form 'GSE' 	dealing with income from 'Gewerbebetrieb'
;;   "DV" for sub-form 'V'   	dealing with income from 'Vermietung und 
;;		Verpachtung'
;;   "DK" for sub-form 'K'   	dealing with income from 'Kapitalvermögen' 
;;		capital and investment
;;   "DF" for sub-form 'FW' 	dealing with 'Förderung von Wohnungseigentum' 
;;
;;   The following codes are only used to show groupings in EST
;;
;;   "DE" heading only for 'Erträge' 	(Income)
;;   "DU" heading only for 'Aufwendungen'  	(Expense)
;;   "DA" heading only for 'Aktiva'  	(Assets)
;;   "DP" heading only for 'Passiva'  	(Liabilities u.a.)
;;
;;    Code N000 continues to be used to signify 'report only, no export'
;;
;;   The following codes were apparently only used to show groupings 
;;	for information in UST
;;
;;   "UST"
;; 
;;   codes should NOT overlap between UST and EST

;; 2019
;; FIXME:   	Explanations need to be more detailed, mostly just copied from 
;;	taxtxf-de_DE for now
;;          	gnucash-docs not updated at this time
;; 2020
;; FIXME:   	Tax form entries (tax entities) are internally given  
;;	tax category codes, as menitioned above
;;	These tax category codes are assigned to accounts by 
;;	selecting the form entry in the 
;;	  'Bearbeiten > Optionen Steuerbericht' menu
;;          	Entries are grouped into logical groups by prefixing the  
;;          	groups with an explanation, which is assigned a tax code
;;	This tax code should NOT be assigned to any account, 
;;	it might crash the tax report

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


(define txf-help-strings
  '(

;; FIXME:	remove duplicate entries "nur zur Gliederung" in txf.scm

;; USt Income 1
    (U00 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (U10 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (U11 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K41 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) an Abnehmer MIT USt-IdNr. (Erlöse)")
    (K44 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) neuer Fahrzeuge an Abnehmer OHNE USt-IdNr. (Erlöse)")
    (K49 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG) (Erlöse)")
    (K43 . "Weitere steuerfreie Umsätze mit Vorsteuerabzug (z.B. Ausfuhrlieferungen, Umsätze nach § 4 Nr. 2 bis 7 UStG) (Erlöse)")
    (U12 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K48 . "Steuerfreie Umsätze ohne Vorsteuerabzug Umsätze nach § 4 Nr. 8 bis 28 UStG (Erlöse)")

;; USt Income 2
    (U20 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K51 . "Steuerpflichtige Umsätze zum Steuersatz von 16 % (bis incl. 2006) (Erlöse)")
    (K81 . "Steuerpflichtige Umsätze zum Steuersatz von 19 % (Erlöse)")
    (K86 . "Steuerpflichtige Umsätze zum Steuersatz von  7 % (Erlöse)")
    (K35 . "Steuerpflichtige Umsätze zu anderen Steuersätzen (Erlöse)")
    (K36 . "Steuerpflichtige Umsätze zu anderen Steuersätzen (Mehrwertsteuer)")
    (U21 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K77 . "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG an Abnehmer MIT USt-IdNr. (Erlöse)")
    (K76 . "Lieferungen land- und forstwirtschaftlicher Betriebe, für die eine Steuer nach § 24 UStG zu entrichten ist (Sägewerkserzeugnisse, Getränke und alkohol. Flüssigkeiten, z.B. Wein) (Erlöse)")
    (K80 . "Lieferungen land- und forstwirtschaftlicher Betriebe, für die eine Steuer nach § 24 UStG zu entrichten ist (Sägewerkserzeugnisse, Getränke und alkohol. Flüssigkeiten, z.B. Wein) (Mehrwertsteuer)")

;; USt Income 3
    (U30 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (U32 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K98 . "Steuerpflichtige innergemeinschaftliche Erwerbe zu anderen Steuersätzen (Gebuchte Mehrwertsteuer)")
    (K96 . "Steuerpflichtige innergemeinschaftliche Erwerbe neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Gebuchte Mehrwertsteuer)")

;; USt Income 4
    (U40 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K42 . "Lieferungen des ersten Abnehmers bei innergemeinschaftlichen Dreiecksgeschäften (§ 25b Abs. 2 UStG) (Erlöse)")
    (K60 . "Steuerpflichtige Umsätze, für die der Leistungsempfänger die Steuer nach § 13b Abs. 5 UStG schuldet (Erlöse)")
    (K21 . "Nicht steuerbare sonstige Leistungen gem. § 18b Satz 1 Nr. 2 UStG (Leistungen EU-Land) (Erlöse)")
    (K45 . "Übrige nicht steuerbare Umsätze (Leistungsort nicht im Inland) (Erlöse)")

;; USt Income 5
    (U50 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K47 . "Im Inland steuerpflichtige sonstige Leistungen von im übrigen Gemeinschaftsgebiet ansässigen Unternehmern (§13b Abs. 1 UStG) (Gebuchte Mehrwertsteuer)")
    (K53 . "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Gebuchte Mehrwertsteuer)")
    (K74 . "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (§ 13b Abs. 2 Nr. 2 und 3 UStG) (Gebuchte Mehrwertsteuer)")
    (K85 . "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Gebuchte Mehrwertsteuer)")

;; USt income 6
    (U70 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K65 . "Steuer infolge Wechsels der Besteuerungsform sowie Nachsteuer auf versteuerte Anzahlungen u. ä. wegen Steuersatzänderung (Gebuchte Mehrwertsteuer --> Mehr- oder ggf. auch Minderbetrag)")
    (K69 . "In Rechnungen unrichtig oder unberechtigt ausgewiesene Steuerbeträge (§ 14c UStG) sowie Steuerbeträge, die nach § 4 Nr. 4a Satz 1 Buchstabe a Satz 2, § 6a Abs. 4 Satz 2, § 17 Abs. 1 Satz 6 oder § 25b Abs. 2 UStG geschuldet werden (Gebuchte Mehrwertsteuer --> Mehrbetrag)")


;; ESt Income 1
    (DE100 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DE000 . "Bei Auswahl dieser Kategorie wird das Konto im Steuerbericht zur Information angezeigt, aber nicht exportiert.")

    (DM600 , "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DM682 . "EStMB Feld 68/2 Erstattete Krankenversicherung" )
    (DM692 . "EStMB Feld 69/2 Erstattete Unfallversicherung")
    (DM702 . "EStMB Feld 70/2 Erstattet Lebensversicherung ohne Vermögenswirksame Leistungen")
    (DM712 . "EStMB Feld 71/2 Erstatt. Haftpflichtversicherung")
    (DM762 . "EStMB Feld 76/2 Erstattete   Kirchensteuer")
    (DM116 . "EStMB Feld 116/2 Erstattete außergewöhnliche Belastungen mit zumutbaren Belastungen")

    (DE001 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")

;; ESt income, form "R"
    (DR000 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DR001 . "Formular R - Renten")

;; ESt income from business, codes DG 100-199, codes selected  do not reflect any form field
    (DG100 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DG101 . "GSE Betriebseinnahmen Gewerbebetrieb")
    (DG102 . "GSE Einkünfte aus Beteiligungen")
    (DG103 . "GSE Einnahmen aus selbst. Arbeit")
    (DG104 . "GSE Einkünfte aus bürgerl. Ges.")

;; ESt income from renting and leasing, form V, codes DV020-271, codes selected from form fields
    (DV000 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DV020 . "V Feld 2 Mieteinnahmen")
    (DV040 . "V Feld 4 Mieteinnahmen verbilligt überlassen")
    (DV050 . "V Feld 5 Einnahmen aus Umlagen")
    (DV061 . "V Feld 6 Mieteinnahmen für frühere Jahre")
    (DV062 . "V Feld 6 Miet-Vorauszahlungen außer Baukostenvorschuß")
    (DV071 . "V Feld 7 Mieteinnahmen von Werbefläche(n)")
    (DV072 . "V Feld 7 Mieteinnahmen von Kiosken")
    (DV073 . "V Feld 7 Mieteinnahmen von anderen Gebäuden")
    (DV074 . "V Feld 7 Erstattete Umsatzsteuer")
    (DV131 . "V Feld 3 Öffentliche Zuschüsse zu Erhaltungsaufwand")
    (DV132 . "V Feld 3 sonstige Aufwendungszuschüsse")
    (DV133 . "V Feld 3 erhaltene Bausparvertragszinsen")
    (DV134 . "V Feld 3 erhaltene Nutzungsentschädigung")
    (DV211 . "V Feld 1-1 Ant Eink Bauherrn/Erwerberg")
    (DV261 . "V Feld 6-1 Einnahmen aus Untervermietung von gemieteten Räumen")
    (DV271 . "V Feld 7-1 Einnahme aus anderer Vermietung & Verpachtung")

;; ESt income continued, capital gains, codes DK 000-299, codes derived from field numbers of the form
    (DK000 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DK041 . "KAP Feld  4-1  Zinsen und andere Erträge-Steuerpflichtige/r")
    (DK042 . "KAP Feld  4-2  Zinsen und andere Erträge-Ehepartner")
    (DK043 . "KAP Feld  4-3  Zinsen und andere Erträge-Freis.")
    (DK044 . "KAP Feld  4-4  Zinsen und andere Erträge-KapESt/ZASt")
    (DK051 . "KAP Feld  5-1  Bausparguthaben-Steuerpflichtiger")
    (DK052 . "KAP Feld  5-2  Bausparguthaben-Ehefrau")
    (DK053 . "KAP Feld  5-3  Bausparguthaben-Freistell.")
    (DK054 . "KAP Feld  5-4  Bausparguthaben-KapESt/ZASt")
    (DK061 . "KAP Feld  6-1  Festverzinsliche Wertpapiere/Investitionsanleihen, Steuerpflichtige/r")
    (DK062 . "KAP Feld  6-2  Festverzinsliche Wertpapiere/Investitionsanleihen, Ehepartner")
    (DK063 . "KAP Feld  6-3  Festverzinsliche Wertpapiere/Investitionsanleihen, Freistell.")
    (DK064 . "KAP Feld  6-4  Festverzinsliche Wertpapiere/Investitionsanleihen, KESt/ZASt")
    (DK065 . "KAP Feld  6-5  Festverzinsliche Wertpapiere/Investitionsanleihen, KSt")
    (DK071 . "KAP Feld  7-1  Tafelg.m.festv.WP-Steuerpflichtige/r")
    (DK072 . "KAP Feld  7-2  Tafelg.m.festv.WP-Ehepartner")
    (DK073 . "KAP Feld  7-3  Tafelg.m.festv.WP-KESt/ZASt")
    (DK081 . "KAP Feld  8-1  And.Kapitalford.-Steuerpflichtige/r")
    (DK082 . "KAP Feld  8-2  And.Kapitalford.-Ehefrau")
    (DK083 . "KAP Feld  8-3  And.Kapitalford.-Freistell.")
    (DK084 . "KAP Feld  8-4  And.Kapitalford.-KESt/ZASt")
    (DK091 . "KAP Feld  9-1  Dividenden a. Aktien-Steuerpflichtige/r")
    (DK092 . "KAP Feld  9-2  Dividenden a. Aktien-Ehepartner")
    (DK093 . "KAP Feld  9-3  Dividenden a. Aktien-Freis.")
    (DK094 . "KAP Feld  9-4  Divid. a. Aktien-KESt/ZASt")
    (DK095 . "KAP Feld  9-5  Dividenden aus Aktien-KSt")
    (DK101 . "KAP Feld 10-1  Wandelanl./Gewinnobl.-Steuerpflichtige/r")
    (DK102 . "KAP Feld 10-2  Wandelanl./Gewinnobl.-Ehepartner")
    (DK103 . "KAP Feld 10-3  Wandelanl./Gewinnob.-Freis.")
    (DK104 . "KAP Feld 10-4  Wandelan/Gewinnob-KESt/ZASt")
    (DK111 . "KAP Feld 11-1  St.pfl.Einn.Lebensv.-Steuerpflichtige/r")
    (DK112 . "KAP Feld 11-2  Stpfl.Einn.Lebensv.-Ehepartner")
    (DK113 . "KAP Feld 11-3  Stpfl.Einn.Lebensv.-Freist.")
    (DK114 . "KAP Feld 11-4  Stpfl.Ein.Lebenv.-KESt/ZAS")
    (DK121 . "KAP Feld 12-1  St.Gesells./part.Darl-Steuerpflichtige/r")
    (DK122 . "KAP Feld 12-2  St.Gesells./part.Darl-Ehefr")
    (DK123 . "KAP Feld 12-3  St.Gesells./part.Darl-Freis")
    (DK124 . "KAP Feld 12-4  St.Ges./part.Darl.KESt/ZASt")
    (DK142 . "KAP Feld 14-2  Beteiligungen-Steuerpflichtige/r")
    (DK143 . "KAP Feld 14-3  Beteiligungen-Ehefrau")
    (DK144 . "KAP Feld 14-4  Beteiligungen-KapESt/ZASt")
    (DK145 . "KAP Feld 14-5  Beteiligungen-KSt")
    (DK151 . "KAP Feld 15-1  Kapitalforderungen ohne Steuerabzüge - Steuerpflichtige/r")
    (DK152 . "KAP Feld 15-2  Kapitalforderungen ohne Steuerabzüge - Ehepartner")
    (DK161 . "KAP Feld 16-1  Erst.zins Finanzamt-Steuerpflichtige/r")
    (DK162 . "KAP Feld 16-2  Erst.zins Finanzamt-Ehepartner")
    (DK201 . "KAP Feld 20-1  Ausländische Kapitalerträge - Steuerpflichtige/r")
    (DK202 . "KAP Feld 20-2  Ausländische Kapitalerträge - Ehepartner")
    (DK203 . "KAP Feld 20-3  Ausländische Kapitalerträge - Freis.")

;; expenses

;; USt
;;  (U30 and 
;;  (U32 already defined in USt income
    (U31 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K91 . "Erwerbe nach § 4b UStG")
    (U32 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K89 . "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 19 % (Aufwand)")
    (K93 . "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von  7 % (Aufwand)")
    (K95 . "Steuerpflichtige innergemeinschaftliche Erwerbe zu anderen Steuersätzen (Aufwand)")
    (K94 . "Steuerpflichtige innergemeinschaftliche Erwerbe neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Aufwand)")

    (U50 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K46 . "Im Inland steuerpflichtige sonstige Leistungen von im übrigen Gemeinschaftsgebiet ansässigen Unternehmern (§13b Abs. 1 UStG) (Aufwand)")
    (K52 . "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Aufwand)")
    (K73 . "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (§ 13b Abs. 2 Nr. 2 und 3 UStG) (Aufwand)")
    (K84 . "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Aufwand)")

    (U60 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (K66 . "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG), aus Leistungen im Sinne des § 13a Abs. 1 Nr. 6 UStG (§ 15 Abs. 1 Satz 1 Nr. 5 UStG) und aus innergemeinschaftlichen Dreiecksgeschäften (§ 25b Abs. 5 UStG) (Vorsteuer allgemein)")
    (K61 . "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen (§ 15 Abs. 1 Satz 1 Nr. 3 UStG) (Gebuchte Vorsteuer)")
    (K62 . "Entrichtete (abziehbare) Einfuhrumsatzsteuer (§ 15 Abs. 1 Satz 1 Nr. 2 UStG) (bezahlte EUSt)")
    (K67 . "Vorsteuerbeträge aus Leistungen im Sinne des § 13b UStG (§ 15 Abs. 1 Satz 1 Nr. 4 UStG) (Gebuchte Vorsteuer)")
    (K63 . "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind (§§ 23 und 23a UStG) (Gebuchte Vorsteuer)")
    (K64 . "Berichtigung des Vorsteuerabzugs - auf Wirtschaftsgüter - (§ 15a UStG) (Gebuchte Vorsteuerberichtigungen --> Mehr- oder Minderbetrag)")
    (K59 . "Vorsteuerabzug für innergemeinschaftliche Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG) sowie von Kleinunternehmern im Sinne des § 19 Abs. 1 UStG (§ 15 Abs. 4a UStG) (Gebuchte/bezahlte Vorsteuer)")

;;  (U70 already defined in ESt income
    (K39 . "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung (1/11) für Dauerfristverlängerung ==> Das Konto »Umsatzsteuer-Vorauszahlung 1/11« darf nur für die letzte Voranmeldung des Besteuerungszeitraums, in der Regel Dezember, dieser Kennzahl zugeordnet werden, da sonst die Verrechnung vorzeitig bzw. gar nicht statt finden würde!")

;; leftovers
    (N000 . "Bei Auswahl dieser Kategorie wird das Konto im Steuerbericht zur Information angezeigt, aber nicht exportiert.")
    (UST . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")


;; expenses ESt

;; expenses info headers only
    (DU000 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DU001 . "Nur Report, kein Export" )
    (DU002 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!" )

 ;; expenses and income (reimbursements) on the Mantelbogen, codes DM

    (DM670 . "EStMB Feld 67   Freiwillige Beiträge zur Rentenversicherung")
    (DM681 . "EStMB Feld 68/1 Gezahlte   Krankenversicherung")
    (DM691 . "EStMB Feld 69/1 Gezahlte   Unfallversicherung")
    (DM701 . "EStMB Feld 70/1 Gezahlte  Lebensversicherung ohne Vermögenswirksame Leistungen")
    (DM711 . "EStMB Feld 71/1 Gezahlte Haftpflichtversicherung")

    (DM733 . "EStMB Feld 73/3 Gezahlte Rente")
    (DM743 . "EStMB Feld 74/3 Dauernde Last")
    (DM750 . "EStMB Feld 75   Unterhaltsleistung")
    (DM761 . "EStMB Feld 76/1 Nachgezahlte Kirchensteuer")
    (DM770 . "EStMB Feld 77   Zins für Nachforderung/Stundung")
    (DM790 . "EStMB Feld 79   Hauswirtschaf. Besch.Verh.")
    (DM800 . "EStMB Feld 80   Steuerberatungskosten")
    (DM820 . "EStMB Feld 82   Eigene Berufsausbildung")
    (DM830 . "EStMB Feld 83   Schulgeld")
    (DM850 . "EStMB Feld 85   Spende: wissenschaftlich, mildtätig, kulturell")
    (DM860 . "EStMB Feld 86   Spende: kirchlich, religiös, gemeinnützig")
    (DM870 . "EStMB Feld 87   Spende an Partei")
    (DM880 . "EStMB Feld 88   Spende an unabhängige Wählervereinigung")
    (DM916 . "EStMB Feld 16/1 Außergew Belastungen mit zumutbaren Belastungen")

;; expenses on form "N"
    (DN000 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DN380 . "N Feld 38 Öffentliche Verkehrsmittel")
    (DN400 . "N Feld 40 Beitrag Berufsverband")
    (DN441 . "N Feld 44 Arbeitsmittel / Computerbedarf")
    (DN442 . "N Feld 44 Arbeitsmittel / Fachliteratur")
    (DN443 . "N Feld 44 Arbeitsmittel / Büromaterial")
    (DN444 . "N Feld 44 Arbeitsmittel / Berufsversich.")
    (DN445 . "N Feld 44 Arbeitsmittel / Berufskleidung")
    (DN446 . "N Feld 44 Arbeitsmittel / Fortbildungsk.")
    (DN447 . "N Feld 44 Arbeitsmittel / Geringwertige Wirtschaftsgüter GWG")
    (DN448 . "N Feld 44 Arbeitsmittel / Kundengeschenk")
    (DN449 . "N Feld 44 Weitere Werbungskosten")
    (DN440 . "N Feld 44 Arbeitsmittel / Arbeitszimmer")


;; form GSE also has some expenses, codes DG 200-299, codes selected  do not reflect any form field
    (DG200 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")
    (DG201 . "GSE Betriebsausgaben Gewerbebetrieb")
    (DG202 . "GSE Betriebsausgaben selbständige Arbeit")

;; expenses related to capital gains, codes DK 251-300, codes selected from 
;; form fields (251-253) or not reflecting any form field (301...)
    (DK000 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")

    (DK251 . "KSO 25-1  Werbungskosten-Beteiligung")
    (DK252 . "KSO 25-2  Werbungskosten-Steuerpflichtiger")
    (DK253 . "KSO 25-3  Werbungskosten-Ehefrau")
    (DK301 . "KAP abgeführte Kapitalertragssteuer laut Bescheinigung")
    (DK302 . "KAP abgeführter Soli laut Bescheinigung")
    (DK303 . "KAP abgeführte Kirchensteuer auf KapitalErtragsSteuer laut Bescheinigung")

    (DK460 . "KAP Feld 46-46 Unterhaltsleistungen")


;; expenses related to income from renting and leasing, code DV 300-699, mostly derived form form field numbers (except 662)
    (DV300 . "Dieser Eintrag dient nur zur Gliederung - Nicht zuordnen!")

    (DV341 . "V Feld 34 Schuldzinsen")
    (DV342 . "V Feld 34 Geldbeschaffungskosten")
    (DV440 . "V Feld 44 Renten")
    (DV450 . "V Feld 45 dauernde Lasten")
    (DV511 . "V Feld 51 Grundsteuer")
    (DV512 . "V Feld 51 Straßenreinigung")
    (DV513 . "V Feld 51 Müllabfuhr")
    (DV521 . "V Feld 52 Wasserversorgung")
    (DV522 . "V Feld 52 Entwässerung")
    (DV523 . "V Feld 52 Hausbeleuchtung")
    (DV531 . "V Feld 53 Heizung")
    (DV532 . "V Feld 53 Warmwasser")
    (DV541 . "V Feld 54 Schornsteinreinigung")
    (DV542 . "V Feld 54 Hausversicherungen")
    (DV551 . "V Feld 55 Hauswart")
    (DV552 . "V Feld 55 Treppenreinigung")
    (DV553 . "V Feld 55 Fahrstuhl")
    (DV560 . "V Feld 56 Sonstiges")
    (DV662 . "V Feld 26-2 Werbungsk Unterverm gem Räu")
    
    ;; codes below were defined by CS / FJS
    
    
    (H001 . "Bei mit \"&lt;\" oder  \"^\" markierten Kategorien wird neben dem Betrag eine Beschreibung exportiert.  \"&lt;\" bedeutet, daß der Name von diesem Konto als 'Payer ID' exportiert wird.  Typischerweise ein Bank-, Aktien- oder Fond-Name.")
    (H003 . "Mit # markierte Kategorien sind noch nicht vollständig implementiert!  Bitte diese Codes nicht verwenden!")
    (H002 . "Bei mit \"&lt;\" oder  \"^\" markierten Kategorien wird neben dem Betrag eine Beschreibung exportiert.  \"^\" bedeutet, daß der Name des übergeordneten Kontos als 'Payer ID' exportiert wird.  Typischerweise ein Bank-, Aktien- oder Fond-Name.")


  )
)
