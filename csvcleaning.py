import pandas as pd

# CSV 파일 경로
csv_file = 'd:\\LDAtopicmodeling\\abstracts_cleaned.csv'
# Excel 파일 경로
excel_file = 'd:\\LDAtopicmodeling\\cleaned_abstracts.xlsx'

# CSV 파일 읽기
df_csv = pd.read_csv(csv_file, header=None)

# Excel 파일 읽기
df_excel = pd.read_excel(excel_file)

# CSV 파일의 내용을 Excel 파일의 B열에 추가
df_excel['B'] = df_csv[0]

# 수정된 내용을 새로운 Excel 파일에 저장
df_excel.to_excel(excel_file, index=False)

print("CSV 파일의 내용이 Excel 파일의 B열에 성공적으로 추가되었습니다.")