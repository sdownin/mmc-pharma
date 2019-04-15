# -*- coding: utf-8 -*-
"""
Created on Mon Apr 15 13:35:56 2019

@author: T430
"""

import re, requests, os
from bs4 import BeautifulSoup
import datetime as dt


class ManualPRCrawler(object):
    
    _DEFAULT_YEAR_STOP = 2007
    
    def __init__(self, name=None, attrs={}, data_dir=None, article_urls=set()):
        self.name = name
        ## attributes
        keys = attrs.keys()
        self.pdf = attrs['pdf'] if 'pdf' in keys else ''
        self.domain = attrs['domain'] if 'domain' in keys else ''
        self.year_stop = attrs['year_stop'] if 'year_stop' in keys else ''
        self.list_base = attrs['list_base'] if 'list_base' in keys else ''
        self.list_url_form = attrs['list_url_form'] if 'list_url_form' in keys else ''
        self.list_re = attrs['list_re'] if 'list_re' in keys else ''
        self.article_re = attrs['article_re'] if 'article_re' in keys else ''
        self.article_base = attrs['article_base'] if 'article_base' in keys else ''
        self.article_url_form = attrs['article_url_form'] if 'article_url_form' in keys else ''
        ## article links
        self.article_urls = article_urls
        self.article_urls_count = len(self.article_urls)
        ## iterator values
        self._reset_counters()
        ## file names
        data_dir = data_dir if data_dir is not None else os.getcwd()
        self._list_txt = os.path.join(data_dir, '%s_article_list.txt' % self.name)
        self._article_csv =  os.path.join(data_dir, '%s_article_dataframe.csv' % self.name)
        ## check urls
        print(' list url format: %s' % self.get_list_url(year=2018, page=2))
        print(' article url format: %s' % self.get_article_url('__TEST_TITLE__'))
    
    def _reset_counters(self):
        self.page = 0
        self.year = dt.datetime.now().year
    
    def get_list_url(self, year=None, page=None):
        year = year if year is not None else self.year
        page = page if page is not None else self.page
        return self.list_url_form.format(base=self.list_base, year=year, page=page)
    
    def get_article_url(self, title):
        return '{domain}/{title}'.format(domain=self.domain, title=title)
    
    def get_max_page(self, url=None):
        url = url if url is not None else self.get_list_url()
        r = requests.get(url)
        soup = BeautifulSoup(r.text)
        links = [x.attrs['href'] for x in soup.select('a[href]')]
        pattern = re.compile('.{0,}%s.{0,}' % self.list_re)
        list_links = [x for x in links if pattern.match(x)]
        page_pattern = re.compile('.+page=')
        page_strings = [page_pattern.sub('',x) for x in list_links if page_pattern.match(x)]
        pages = [int(x) for x in page_strings if isinstance(x, (str))]
        return max(pages) if pages else 0
    
    def extract_article_links(self, url=None):
        url = url if url is not None else self.get_list_url()
        r = requests.get(url)
        soup = BeautifulSoup(r.text)
        links = [x.attrs['href'] for x in soup.select('a[href]')]
        pattern = re.compile('.{0,}%s.{0,}' % self.article_re)
        rel_links = [x for x in links if pattern.match(x)]
        return [self.get_article_url(x) for x in rel_links]
    
    def fetch_article_urls(self, year_start=None, year_stop=None):
        """ extract article links by iterating over archive year and pages
             - from current year back to earliest available:  (2019-->2007)
             - from first page to last available, inclusive year_stop + 1:  (0-->74)
        """
        start_urls_len = len(self.article_urls)
        year_start = year_start if year_start is not None else self.year
        year_stop  = year_stop  if year_stop  is not None else self._DEFAULT_YEAR_STOP
        print(' fetching %s: years %s-%s' % (self.name, year_start, year_stop))
        
        with open(self._list_txt, 'w') as f:
            
            for year in range(year_start, year_stop - 1, -1):
                if year > year_start or year < year_stop:
                    break
                self.year = year
                list_year_url = self.get_list_url(year=self.year, page=0)
                max_page = self.get_max_page(list_year_url)
                print('  year %s pages: %s' % (self.year, max_page))
                
                for page in range(max_page + 1):
                    self.page = page
                    list_year_page_url = self.get_list_url(year=self.year, page=self.page)
                    links = self.extract_article_links(list_year_page_url)
                    if links: 
                        old_count = self.article_urls_count
                        self.article_urls.update(links) ## set update
                        self.article_urls_count = len(self.article_urls)
                        if self.article_urls_count > old_count:
                            url_string = '\n '.join(['"%s"' % x for x in self.article_urls])
                            f.writelines(url_string)
                        if self.page % 5 == 0:
                            print('   page %s total urls: %s' % (self.page, len(self.article_urls)))
                    
        print(' fetched %s article urls' % (len(self.article_urls) - start_urls_len))
        
    
    
config = {
    'pfizer': {
        'domain':'https://pfizer.com',
        'pdf': 0,
        'year_stop':2007,
        'list_base':'https://www.pfizer.com/news/press-release/press-releases-archive',
        'list_url_form':'{base}?field_press_release_date_value%5Bvalue%5D%5Byear%5D={year}&page={page}',
        'list_re':'press-releases-archive',
        'article_base':'https://www.pfizer.com/news/press-release/press-release-detail',
        'article_url_form':'{base}/{title}',
        'article_re':'press-release-detail'
    }
}

work_dir = 'C:\\Users\\T430\\Google Drive\\PhD\\Research\\MMC\\pharma_encounters\\mmc-pharma'
data_dir = os.path.join(work_dir, 'analysis_data')


firm = 'pfizer'
clr = ManualPRCrawler(firm, config[firm], data_dir, _article_urls)
clr.fetch_article_urls(year_start=2016)
print(clr.article_urls)

_article_urls = set(clr.article_urls)

#     "sanofi":"https://mediaroom.sanofi.com/en/press-releases/",
#    "pfizer":"https://www.pfizer.com/news/press-release/press-release-detail/augustus_demonstrates_favorable_safety_results_of_eliquis_versus_vitamin_k_antagonists_in_non_valvular_atrial_fibrillation_patients_with_acute_coronary_syndrome_and_or_undergoing_percutaneous_coronary_intervention"
#    "novartis":"https://www.novartis.com/news/media-releases/novartis-joins-global-chagas-disease-coalition-and-also-announces-first-multinational-prospective-randomized-study-people-chronic-chagas-cardiomyopathy"
#    "jnj":"https://www.jnj.com/media-center/press-releases"
#    "gsk":"https://www.gsk.com/en-gb/media/press-releases/viiv-healthcare-presents-positive-48-week-data-from-two-pivotal-phase-iii-studies/"
#    "amgen":"https://www.amgen.com/media/news-releases/2019/03/amgen-announces-new-fouryear-outcomes-study-to-examine-longterm-effects-of-repatha-evolocumab-in-highrisk-cardiovascular-disease-cvd-patients-without-prior-heart-attack-or-stroke/"
#    "biogen":"http://investors.biogen.com/news-releases/news-release-details/biogen-enters-agreement-sell-its-large-scale-biologics"
#    "eli_lilly":"https://investor.lilly.com/news-releases/news-release-details/lillys-cyramzar-ramucirumab-phase-3-relay-trial-met-primary"
#    "abbview":"https://news.abbvie.com/news/press-releases/abbvie-announces-multiple-milestones-for-phase-3-cll14-venetoclax-study-fixed-duration-treatment-in-previously-untreated-chronic-lymphocytic-leukemia-patients.htm"
#    "merck":"https://www.mrknewsroom.com/news-release/research-and-development-news/european-commission-approves-mercks-keytruda-pembrolizu-0"
#    "abbott":"https://abbott.mediaroom.com/2019-03-17-New-Late-Breaking-Analyses-from-Landmark-COAPT-TM-Trial-Show-Benefits-of-Abbotts-MitraClip-TM-Device"
#    "gilead":"https://www.gilead.com/news-and-press/press-room/press-releases/2019/3/gilead-presents-data-on-biktarvy-bictegravir-emtricitabine-and-tenofovir-alafenamide-in-virologically-suppressed-adults-including-those-with-pre"
#    "bristol-myers_squibb":"https://news.bms.com/press-release/bmy/augustus-demonstrates-favorable-safety-results-eliquis-versus-vitamin-k-antagonist"
#    "astrazeneca":"https://www.astrazeneca.com/content/astraz/media-centre/press-releases/2019/first-sub-analyses-from-the-declare-timi-58-trial-further-support-the-cardiovascular-effects-of-farxiga-in-type-2-diabetes-18032019.html"
#    "roche":"https://www.roche.com/media/releases/med-cor-2019-03-11b.htm"
#    "bayer":"https://media.bayer.com/baynews/baynews.nsf/id/Phase-III-clinical-trial-Bayers-Nifurtimox-demonstrates-safety-efficacy-formulation-treat-children?Open&parent=news-overview-category-search-en&ccm=020"
#    "novo_nordisk":"https://www.novonordisk.com/content/Denmark/HQ/www-novonordisk-com/en_gb/home/media/news-details.2238847.html"

