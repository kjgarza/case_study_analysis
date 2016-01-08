import scrapy

class BlogSpider(scrapy.Spider):
    name = 'scrappying-seek'
    start_urls = ['https://seek.sysmo-db.org/publications?page=all']

    def parse(self, response):
        for url in response.css('div a::attr("href")').re(r'.*/\d\d\d\d/\d\d/$'):
            yield scrapy.Request(response.urljoin(url), self.parse_date_pub)

    def parse_date_pub(self, response):
        for date_pub in response.css('div.list_items_container > div.panel.panel-default.list_item > div.panel-heading > div.pull-right.small > a::text').extract():
            yield {'date_pub': date_pub}
