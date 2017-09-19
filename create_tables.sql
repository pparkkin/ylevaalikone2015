
CREATE TABLE vaalipiirit (
  id integer PRIMARY KEY,
  name text NOT NULL UNIQUE
);

CREATE TABLE puolueet (
  id integer PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE sukupuolet (
  id integer PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE kotikunnat (
  id integer PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE kielet (
  id integer PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE koulutukset (
  id integer PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE uskonnolliset_yhteisot (
  id integer PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE kokemukset (
  id integer PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE vaalibudjetit (
  id integer PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE ulkopuolisen_rahoituksen_osuudet (
  id integer PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE ulkopuolisen_rahoituksen_lahteet (
  id integer PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE vuositulot (
  id integer PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE sijoitukset (
  id integer PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE kielitaidot ( -- many-to-many
  vastaaja_id integer NOT NULL,
  kieli_id integer NOT NULL,
  PRIMARY KEY (vastaaja_id, kieli_id),
  FOREIGN KEY (vastaaja_id) REFERENCES vastaajat (id),
  FOREIGN KEY (kieli_id) REFERENCES kielet (id)
);

CREATE TABLE poliittiset_kokemukset ( -- many-to-many
  vastaaja_id integer NOT NULL,
  kokemus_id integer NOT NULL,
  PRIMARY KEY (vastaaja_id, kokemus_id),
  FOREIGN KEY (vastaaja_id) REFERENCES vastaajat (id),
  FOREIGN KEY (kokemus_id) REFERENCES kokemukset (id)
);

CREATE TABLE vastaajat (
  id integer PRIMARY KEY,
  sukunimi text,
  etunimi text,
  puolue integer, -- FK
  ika integer,
  sukupuoli integer, -- FK
  kansanedustaja integer,
  vastattu text,
  valittu integer,
  sitoutumaton integer,
  kotikunta integer, -- FK
  ehdokasnumero integer
  miksi_eduskuntaan text,
  mita_edistaa text,
  vaalilupaus1 text,
  vaalilupaus2 text,
  vaalilupaus3 text,
  aidinkieli integer, -- FK
  kotisivu text,
  facebook text,
  twitter text,
  lapsia integer,
  perhe text,
  vapaa_ajalla text,
  tyonantaja text,
  ammattiasema text,
  ammatti text,
  koulutus integer, -- FK
  uskonnollinen_yhteiso integer, -- FK
  puolueen_jasen integer,
  vaalibudjetti integer, -- FK
  ulkopuolisen_rahoituksen_osuus integer, -- FK
  ulkopuolisen_rahoituken_lahde integer, -- FK
  sidonnaisuudet text,
  vuositulot integer, -- FK
  sijoitukset integer, -- FK

  FOREIGN KEY (puolue) REFERENCES puolueet (id),
  FOREIGN KEY (sukupuoli) REFERENCES sukupuolet (id),
  FOREIGN KEY (kotikunta) REFERENCES kotikunnat (id),
  FOREIGN KEY (aidinkieli) REFERENCES kielet (id),
  FOREIGN KEY (koulutus) REFERENCES koulutukset (id),
  FOREIGN KEY (uskonnollinen_yhteiso) REFERENCES uskonnolliset_yhteisot (id),
  FOREIGN KEY (vaalibudjetti) REFERENCES vaalibudjetit (id),
  FOREIGN KEY (ulkopuolisen_rahoituksen_osuus) REFERENCES ulkopuolisen_rahoituksen_osuudet (id),
  FOREIGN KEY (ulkopuolisen_rahoituken_lahde) REFERENCES ulkopuolisen_rahoituksen_lahteet (id),
  FOREIGN KEY (vuositulot) REFERENCES vuositulot (id),
  FOREIGN KEY (sijoitukset) REFERENCES sijoitukset (id)
);

CREATE TABLE kysymykset (
  id integer PRIMARY KEY,
  kysymys text NOT NULL
);

CREATE TABLE vastaukset (
  id integer PRIMARY KEY,
  vastaus text NOT NULL
);

CREATE TABLE vastaaja_vastaukset (
  vastaaja_id integer NOT NULL,
  kysymys_id integer NOT NULL,
  vastaus_id integer NOT NULL,
  PRIMARY KEY (vastaaja_id, kysymys_id, vastaus_id),
  FOREIGN KEY (vastaaja_id) REFERENCES vastaajat (id),
  FOREIGN KEY (kysymys_id) REFERENCES kysymykset (id),
  FOREIGN KEY (vastaus_id) REFERENCES vastaukset (id)
);

CREATE TABLE kommentit (
  vastaaja_id integer NOT NULL,
  kysymys_id integer NOT NULL,
  kommentti text,
  PRIMARY KEY (vastaaja_id, kysymys_id),
  FOREIGN KEY (vastaaja_id) REFERENCES vastaajat (id),
  FOREIGN KEY (kysymys_id) REFERENCES kysymykset (id)
);
