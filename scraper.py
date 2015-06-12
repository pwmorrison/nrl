from bs4 import BeautifulSoup
import urllib2
import datetime
import os

# Map for the team names that we don't support.
nrl_team_map = {
    "Canterbury-Bankstown" : "CanterburyBankstown",
    "Gold Coast" : "GoldCoast",
    "North Queensland" : "NorthQueensland",
    "South Sydney" : "SouthSydney",
    "St George Illawarra" : "StGeorgeIllawarra",
    "Sydney Roosters" : "Sydney",
    "Warriors" : "NewZealand",
    "Wests Tigers" : "Wests"
}

# Mapping between month name and number.
month_map = {
    "jan" : 1,
    "feb" : 2,
    "mar" : 3,
    "apr" : 4,
    "may" : 5,
    "jun" : 6,
    "jul" : 7,
    "aug" : 8,
    "sep" : 9,
    "oct" : 10,
    "nov" : 11,
    "dec" : 12
}

"""
Outputs the given soup table to a CSV file in the current working directory with the given name.
"""
def output_table_to_csv(table, name):
    csv_file = open(name + ".csv", 'w')    

    rows = table.find_all("tr")
    print len(rows)
    #print table
    for row in rows:
        csv_str = ""
        cells = row.find_all(["th", "td"])
        for cell in cells:
            # Get the colspan of the cell. If there isn't one specified, it is 0.
            colspan = 1
            try:
                colspan = int(cell["colspan"])
            except:
                pass
            
            # Get the value of the cell.
            cell_value = " "
            if cell.string != None:
                print cell.get_text()
                try:
                    cell_value = str(cell.get_text())#string)
                except:
                    # This happens when the cell contains other HTML, such as links on the play-by-play pages.
                    # Will need to recursively convert these to text.
                    cell_value = "XXX"
                    
            cell_value = cell.get_text()
            csv_str += cell_value + ", "
            
            # Output empty cells for the colspan.
            #print colspan
            for i in range(1, colspan):
                csv_str += ", "
            
        csv_file.write(csv_str + "\n")
    csv_file.close()

    
"""
Extracts the data from the tables in the given URL, writing it to CSV files.
"""
def extract_data_from_url(url):
    f = urllib2.urlopen(url)
    html = f.read()
    f.close()
    soup = BeautifulSoup(html)
    #print(soup.prettify())
    #print soup.original_encoding

    tables = soup.find_all("table", {"class" : "stats_table"})
    for table in tables:
        # Get the table ID, and skip over those that don't have one.
        id = "None"
        cls = "None"
        try:
            id = table["id"]
            cls = table["class"]
        except:
            continue
        print id, cls
    
        output_table_to_csv(table, id)
        
        
"""
Extracts the data from the tables in the given URL, writing it to CSV files.
"""
def extract_pbp_data_from_url(url):
    f = urllib2.urlopen(url)
    html = f.read()
    
    html = html.replace("<th", "<td")
    html = html.replace("</th", "</td")
    
    # Replace all th tags with td, to avoid a mismatched tag in the page.
    #html_new = ""#file("new_html")
    #for line in html:
    #    new_line = line.replace("<th", "<td")
    #    new_line = new_line.replace("</th", "</td")
    #    print new_line
    #    html_new += new_line
    
    f.close()
    soup = BeautifulSoup(html)
    #print(soup.prettify())
    #print soup.original_encoding

    tables = soup.find_all("table", {"class" : "stats_table"})
    for table in tables[3:]:
        # Get the table ID, and skip over those that don't have one.
        id = "None"
        cls = "None"
        try:
            id = table["id"]
        except:
            # The table that doesn't have an ID is the play-by-play one.
            id = "PBP"
        print id
    
        output_table_to_csv(table, id)
        
        
        
def get_box_score_links(url):
    f = urllib2.urlopen(url)
    html = f.read()
    f.close()
    soup = BeautifulSoup(html)
    
    # Find the links with display text "Box Score".
    base_url = "http://www.basketball-reference.com"
    
    box_score_links = []
    links = soup.find_all("a", text="Box Score")
    for link in links:
        box_score_links.append(base_url + link["href"])
        #print link.text, link["href"]
    
    return box_score_links
    
def form_date_url(date):
    base_url = "http://www.basketball-reference.com"
    url = base_url + "/boxscores/index.cgi?month=" + str(date.month) + "&day=" + str(date.day) + "&year=" + str(date.year)
    return url
    
def extract_box_scores(date):
    date_url = form_date_url(date)
    print date_url
    
    # Create a directory in which to store the files
    date_dir = os.path.join(os.getcwd(), date.isoformat())
    print date_dir
    
    old_cwd = os.getcwd()
    if not os.path.exists(date_dir):
        os.mkdir(date_dir)
    os.chdir(date_dir)
    
    box_score_links = get_box_score_links(date_url)
    print "Found box score links:", box_score_links

    game_num = 0
    for box_score_link in box_score_links:
        game_dir = os.path.join(date_dir, str(game_num))
        if not os.path.exists(game_dir):
            os.mkdir(game_dir)
        os.chdir(game_dir)
            
        extract_data_from_url(box_score_link)#"http://www.basketball-reference.com/boxscores/201305260IND.html")
        
        game_num += 1
        os.chdir(date_dir)
        
    os.chdir(old_cwd)
        
    
def extract_box_scores_range(start_date, end_date):
    assert(start_date <= end_date)
    print "Extracting box scores from", start_date, "to", end_date
    
    one_day = datetime.timedelta(days=1)
    
    # Loop through the dates in the given range.
    current_date = start_date
    while current_date <= end_date:
        print current_date
        
        extract_box_scores(current_date)
        
        current_date += one_day
    
def nrlstats_form_season_url(year):
    #base_url = "http://live.nrlstats.com/nrl/season"
    base_url = "http://web.archive.org/web/20080718185646/http://www.nrlstats.com/season"
    url = base_url + str(year) + ".html"
    url = "http://web.archive.org/web/20080718185646/http://www.nrlstats.com/season2007/index.html"
    return url
    
def get_nrlstats_row_values(row):
    """
    Gets the values from a row.
    """
    cols = row.find_all('td')
    cols_2 = row.find_all('th')
    
    if len(cols) == 0 and len(cols_2) == 0:
        return (None, None, None)
    
    if len(cols) == 0:
        # There were no td tags, so get all values from th tags.
        val_1_strings = cols_2[0].strings
        val_2_strings = cols_2[2].strings
        stat_strings = cols_2[1].strings
    elif len(cols_2) > 0:
        # There was a 'th' col.
        val_1_strings = cols[0].strings
        val_2_strings = cols[1].strings
        stat_strings = cols_2[0].strings
    elif len(cols_2) == 0: 
       # There was no 'th' col.
        val_1_strings = cols[0].strings
        val_2_strings = cols[2].strings
        stat_strings = cols[1].strings
        
    val_1 = ""
    for x in val_1_strings:
        val_1 = val_1 + x + " "
    val_1 = val_1.strip()
        
    val_2 = ""
    for x in val_2_strings:
        val_2 = val_2 + x + " "
    val_2 = val_2.strip()
    
    stat = ""
    for x in stat_strings:
        stat = stat + x + " "
    stat = stat.strip()
    
    return (stat, val_1, val_2)     

    
def get_nrlstats_game_stats(div, date, teams):
    """
    Extracts the game stats table, given the game stats div.
    """
    table_divs = div.find_all('div')
    for table_div in table_divs:
        if 'id' not in table_div.attrs.keys():
            continue
        print table_div['id']

        if table_div['id'] == "tab-mdHalf-0-data":
            # Total game stats.
            file = open("game_stats_total.csv", 'w')
        elif table_div['id'] == "tab-mdHalf-1-data":
            # First half game stats.
            file = open("game_stats_first_half.csv", 'w')
        elif table_div['id'] == "tab-mdHalf-2-data":
            # Second half game stats.
            file = open("game_stats_second_half.csv", 'w')
        else:
            continue
            
        rows = table_div.find_all('tr')
        
        # Each row (except the heading row) contains a stat.
        for row in rows[0:]:
            print row
            
            stat, val_1, val_2 = get_nrlstats_row_values(row)
            
            if stat == None:
                continue
            
            string = stat + ", " + val_1 + ", " + val_2 + "\n"
            file.write(string);
        
        file.close()
        
def get_nrlstats_team_stats(div, date, teams):
    """
    Extracts the team stats table, given the team stats div.
    """
    table_divs = div.find_all('div')
    for table_div in table_divs:
        if 'id' not in table_div.attrs.keys():
            continue
        print table_div['id']

        if table_div['id'] == "tab-tsHalf-0-data":
            # Total game stats.
            file = open("team_stats_total.csv", 'w')
        elif table_div['id'] == "tab-tsHalf-1-data":
            # First half game stats.
            file = open("team_stats_first_half.csv", 'w')
        elif table_div['id'] == "tab-tsHalf-2-data":
            # Second half game stats.
            file = open("team_stats_second_half.csv", 'w')
        else:
            continue
            
        rows = table_div.find_all('tr')
        
        # Each row (except the heading row) contains a stat.
        for row in rows[0:]:
            print row
            
            stat, val_1, val_2 = get_nrlstats_row_values(row)
            
            if stat == None:
                continue
            
            string = stat + ", " + val_1 + ", " + val_2 + "\n"
            file.write(string);
        
        file.close()
        
def get_nrlstats_player_stats(div, date, teams, csv_name):
    """
    Extracts a player stats table.
    """
    file = open(csv_name, 'w')
                
    
    rows = div.find_all('tr')
    
    # Each row (except the heading row) contains a stat.
    for row in rows[0:]:
        cols = row.find_all('th')
        if len(cols) == 0:
            cols = row.find_all('td')
    
        for col in cols:
            val_1 = ""
            for x in col.strings:
                val_1 = val_1 + x + " "
            val_1 = val_1.strip()
            file.write(val_1)
            if col != cols[-1]:
                file.write(', ')
            
        file.write('\n') 
    
    file.close()
    
def get_nrlstats_game_stats(div, date, teams, csv_name):
    """
    Extracts a player stats table.
    """
    file = open(csv_name, 'w')
                
    
    rows = div.find_all('tr')
    
    # Each row (except the heading row) contains a stat.
    for row in rows[0:]:
        cols = row.find_all(['th', 'td'])
        #if len(cols) == 0:
        #    cols = row.find_all('td')
        #print cols
        for col_num in range(len(cols)):
            col = cols[col_num]
        #for col in cols:
            val_1 = ""
            
            col_strings = []
            for x in col.strings:
                col_strings.append(x)
                
            for string_num in range(len(col_strings)):
            #for x in col.strings:
                x = col_strings[string_num]
                
                # Replace commas with semi-colons, to avoid going to the next column.
                x = x.replace(",", ";")
                
                val_1 = val_1 + x 
                if string_num != (len(col_strings) - 1):
                    val_1 += " | "
            val_1 = val_1.strip()
            file.write(val_1)
            if col_num != (len(cols)-1):
                file.write(', ')
            
        file.write('\n') 
    
    file.close()
        
def get_nrlstats_match(url, date, teams, year):
    f = urllib2.urlopen(url)
    html = f.read()
    f.close()
    soup = BeautifulSoup(html)
    
    # Create a new directory for this match.
    day_of_month = int(date.split('_')[0])
    month_str = date.split('_')[1]
    month = month_map[month_str.lower()]
    date_str = str(year) + str("%02d" % month) + str("%02d" % day_of_month)
    print date_str
    
    match_dir = os.path.join(os.getcwd(), date_str + "_" + teams[0] + "_" + teams[1])
    print match_dir
    
    old_cwd = os.getcwd()
    if not os.path.exists(match_dir):
        os.mkdir(match_dir)
    os.chdir(match_dir)
    
    tmp_file = open("webpage_pretty.txt", 'w')
    tmp_file.write(soup.prettify())
    tmp_file.close()
   
    # Write out the html to a file.
    html_file = open("webpage.html", 'w')
    html_file.write(html)
    html_file.close()
    
    if False:
        # Loop through the divs that contain tables.
        divs = soup.find_all('div')
        for div in divs:
            if 'class' not in div.attrs.keys():
                continue
            if 'm_nrl' not in div['class']:
                continue
            
            # For the div, find all the sub-divs and extract information.
            table_divs = div.find_all('div')
            for table_div in table_divs:
                if 'class' not in table_div.attrs.keys():
                    continue
                if 'm_h' in table_div['class']:
                    # This is the heading of the table.
                    heading = table_div.span.string
                    if heading == None:
                        continue
                    heading = heading.strip()
                    print heading
                    if heading == "Game Stats":
                        pass#get_nrlstats_game_stats(div, date, teams)
                    elif heading == "Team Stats":    
                        pass#get_nrlstats_team_stats(div, date, teams)
                    elif heading == "Player Stats":
                        pass#get_nrlstats_player_stats(div, date, teams)
        
    divs = soup.find_all('div')
    for div in divs:
        if 'id' not in div.attrs.keys():
            continue
        #print div['id']
        
        # Summary data.
        if div['id'] == "tab-ps-0-summary-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_summary_total.csv")
        if div['id'] == "tab-ps-1-summary-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_summary_first_half.csv")
        if div['id'] == "tab-ps-2-summary-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_summary_second_half.csv")
            
        # Points data.
        if div['id'] == "tab-ps-0-points-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_points_total.csv")
        if div['id'] == "tab-ps-1-points-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_points_first_half.csv")
        if div['id'] == "tab-ps-2-points-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_points_second_half.csv")
            
        # Runs data.
        if div['id'] == "tab-ps-0-runs-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_runs_total.csv")
        if div['id'] == "tab-ps-1-runs-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_runs_first_half.csv")
        if div['id'] == "tab-ps-2-runs-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_runs_second_half.csv")
            
        # Tackles data.
        if div['id'] == "tab-ps-0-tackles-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_tackles_total.csv")
        if div['id'] == "tab-ps-1-tackles-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_tackles_first_half.csv")
        if div['id'] == "tab-ps-2-tackles-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_tackles_second_half.csv")
            
        # Kicks data.
        if div['id'] == "tab-ps-0-kicks-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_kicks_total.csv")
        if div['id'] == "tab-ps-1-kicks-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_kicks_first_half.csv")
        if div['id'] == "tab-ps-2-kicks-data":
            get_nrlstats_player_stats(div, date, teams, "player_stats_kicks_second_half.csv")
    
        # Team stats. 
        if div['id'] == "tab-tsHalf-0-data":
            get_nrlstats_game_stats(div, date, teams, "team_stats_total.csv")
        if div['id'] == "tab-tsHalf-1-data":
            get_nrlstats_player_stats(div, date, teams, "team_stats_first_half.csv")
        if div['id'] == "tab-tsHalf-2-data":
            get_nrlstats_player_stats(div, date, teams, "team_stats_second_half.csv")
            
        # Game stats. 
        if div['id'] == "tab-mdHalf-0-data":
            get_nrlstats_game_stats(div, date, teams, "game_stats_total.csv")
        if div['id'] == "tab-mdHalf-1-data":
            get_nrlstats_game_stats(div, date, teams, "game_stats_first_half.csv")
        if div['id'] == "tab-mdHalf-2-data":
            get_nrlstats_game_stats(div, date, teams, "game_stats_second_half.csv")
            
        if div['id'] == "page-scorecard-data":
            get_nrlstats_game_stats(div, date, teams, "game_scorecard.csv")
            
def get_nrlstats_season_links(url, year):
    print url
    f = urllib2.urlopen(url)
    html = f.read()
    f.close()
    soup = BeautifulSoup(html)
    
    tmp_file = open("webpage_pretty.txt", 'w')
    tmp_file.write(soup.prettify())
    tmp_file.close()
    
    # Write out the html to a file.
    html_file = open("webpage.html", 'w')
    html_file.write(html)
    html_file.close()
   
    # Find all the divs that contain match tables.
    divs = soup.find_all('div')
    match_divs = []
    for div in divs:
        #print div
        if 'class' not in div.attrs.keys():
            continue
        #print div['class']
        if 'm_nrl' not in div['class'] and 'm_5' not in div['class']:
            continue
        print div['class']
        match_divs.append(div)
    
    # Extract the details from each match div.
    for match_div in match_divs:
        print match_div
        divs = match_div.find_all('div')
        for div in divs:
            #print div['class']
            # The heading of each table, which names the round.
            if 'm_h' in div['class']:
                round = div.span.string
            # The contents of each table.
            if 'm_b' in div['class']:
                rows = div.find_all('tr')
                # Each row (except the heading row) contains a game.
                for row in rows[1:]:
                    cols = row.find_all('td')
                    date = cols[0].string.replace(' ', '_')
                    #print date
                    teams = cols[1].string.split('v')
                    team_names = []
                    for team in teams:
                        team = team.strip()
                        if team in nrl_team_map.keys():
                            team = nrl_team_map[team]
                        team_names.append(team)
                    match_link = cols[1].a.attrs['href']
                    score = cols[2]
                    status = cols[3]
                    report_links = cols[4]
                    
                    print "Team names:", team_names
                    
                    
                    
                    # Extract the match figures from the teams link.
                    base_url = ""                
                    base_url = "http://live.nrlstats.com"
                    match_url = base_url + match_link
                    
                    
                    working_dir = os.getcwd()
                    get_nrlstats_match(match_url, date, team_names, year)
                    os.chdir(working_dir)                
                    
                    #return
                    
                #print rows[0]
        #print divs

    return
        
def extract_nrlstats_season(year):
    """
    Extracts the NRL stats for a given season (year).
    """
    season_url = nrlstats_form_season_url(year)
    print "Season URL:", season_url
    
    # Create a directory in which to store the files
    year_dir = os.path.join(os.getcwd(), str(year))
    print "Year directory:", year_dir
    old_cwd = os.getcwd()
    if not os.path.exists(year_dir):
        os.mkdir(year_dir)
    os.chdir(year_dir)
    
    # Get the statistics for the season.
    get_nrlstats_season_links(season_url, year)
    
    return
    
    box_score_links = get_box_score_links(date_url)
    print "Found box score links:", box_score_links

        
if __name__ == "__main__":

    # This page gives the listings of games on a given date:
    # Link with two games: "http://www.basketball-reference.com/boxscores/index.cgi?month=5&day=16&year=2013"
    # Link with no games: "http://www.basketball-reference.com/boxscores/index.cgi?month=5&day=17&year=2013"
    #date_url = "http://www.basketball-reference.com/boxscores/index.cgi?month=5&day=17&year=2013"

    #extract_pbp_data_from_url("http://www.basketball-reference.com/boxscores/pbp/201305140SAS.html")
        
    
    #extract_nrlstats_season(2013)
    extract_nrlstats_season(2007)
    #extract_nrlstats_season(2015)
        
    #start_date = datetime.date(2013, 5, 14)
    #end_date = datetime.date(2013, 5, 16) # datetime.date.today()
    #extract_box_scores_range(start_date, end_date)