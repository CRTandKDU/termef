import requests
import json

def main ():
    # The API endpoint
    # query = '?q=' + "quantique" + '&s=1&mode=exact&field=label';
    # url   = "https://terminologie.finances.gouv.fr/search/infobox" + query

    url = "https://voc.finances.gouv.fr/individual/concept-SG-informatique"

    # A GET request to the API
    response = requests.get(url)

    # Print the response
    response_json = response.json()

    # json_object = json.loads( response_json ["items"][0] )
    json_formatted_str = json.dumps(response_json, indent=2, ensure_ascii=False)
    print(json_formatted_str)    


if __name__ == '__main__':
    main()
