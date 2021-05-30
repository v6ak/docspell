FROM alpine:latest

ARG version=
ARG joex_url=
ARG UNO_URL=https://raw.githubusercontent.com/unoconv/unoconv/0.9.0/unoconv

ENV JAVA_OPTS="-Xmx1536M"

RUN apk add --no-cache openjdk11 \
    tzdata \
    bash \
    curl \
    ghostscript \
    tesseract-ocr \
    tesseract-ocr-data-deu \
    tesseract-ocr-data-fra \
    tesseract-ocr-data-ita \
    tesseract-ocr-data-spa \
    tesseract-ocr-data-por \
    tesseract-ocr-data-ces \
    tesseract-ocr-data-nld \
    tesseract-ocr-data-dan \
    tesseract-ocr-data-fin \
    tesseract-ocr-data-nor \
    tesseract-ocr-data-swe \
    tesseract-ocr-data-rus \
    tesseract-ocr-data-ron \
    tesseract-ocr-data-lav \
    unpaper \
    wkhtmltopdf \
    libreoffice \
    ttf-droid-nonlatin \
    ttf-droid \
    ttf-dejavu \
    ttf-freefont \
    ttf-liberation \
    libxml2-dev \
    libxslt-dev \
    pngquant \
    zlib-dev \
    g++ \
    qpdf \
    py3-pip \
    python3-dev \
    libffi-dev\
    qpdf-dev \
    openssl-dev \
    ocrmypdf \
  && pip3 install --upgrade pip \
  && pip3 install ocrmypdf \
  && curl -Ls $UNO_URL -o /usr/local/bin/unoconv \
  && chmod +x /usr/local/bin/unoconv \
  && apk del curl libxml2-dev libxslt-dev zlib-dev g++ python3-dev py3-pip libffi-dev qpdf-dev openssl-dev \
  && ln -s /usr/bin/python3 /usr/bin/python

WORKDIR /opt
RUN wget ${joex_url:-https://github.com/eikek/docspell/releases/download/v$version/docspell-joex-$version.zip} && \
  unzip docspell-joex-*.zip && \
  rm docspell-joex-*.zip && \
  ln -snf docspell-joex-* docspell-joex

COPY joex-entrypoint.sh /opt/joex-entrypoint.sh

ENTRYPOINT ["/opt/joex-entrypoint.sh"]
EXPOSE 7878

HEALTHCHECK --interval=1m --timeout=10s --retries=2 --start-period=10s \
  CMD pgrep -f joex/lib
