$PBExportHeader$w_info_bisemanal.srw
forward
global type w_info_bisemanal from w_para_informes
end type
type st_2 from statictext within w_info_bisemanal
end type
type st_3 from statictext within w_info_bisemanal
end type
type st_4 from statictext within w_info_bisemanal
end type
type st_6 from statictext within w_info_bisemanal
end type
type st_1 from statictext within w_info_bisemanal
end type
type uo_seltemporada from uo_seleccion_paramtemporada within w_info_bisemanal
end type
type em_semana from editmask within w_info_bisemanal
end type
type em_fecha from editmask within w_info_bisemanal
end type
type st_7 from statictext within w_info_bisemanal
end type
type rb_spacking from radiobutton within w_info_bisemanal
end type
type rb_cpacking from radiobutton within w_info_bisemanal
end type
type st_9 from statictext within w_info_bisemanal
end type
type dw_1 from uo_dw within w_info_bisemanal
end type
type uo_selespecie from uo_seleccion_especie within w_info_bisemanal
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_bisemanal
end type
end forward

global type w_info_bisemanal from w_para_informes
integer width = 2446
integer height = 1656
boolean minbox = false
boolean maxbox = false
st_2 st_2
st_3 st_3
st_4 st_4
st_6 st_6
st_1 st_1
uo_seltemporada uo_seltemporada
em_semana em_semana
em_fecha em_fecha
st_7 st_7
rb_spacking rb_spacking
rb_cpacking rb_cpacking
st_9 st_9
dw_1 dw_1
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
end type
global w_info_bisemanal w_info_bisemanal

type variables
uo_nrosemana	iuo_semana
end variables

forward prototypes
public function boolean existeagronomo (integer ai_agronomo)
end prototypes

public function boolean existeagronomo (integer ai_agronomo);String ls_nom_agr

SELECT agro_nombre
  INTO :ls_nom_agr
  FROM dba.agronomos
  WHERE agro_codigo  = :ai_agronomo;

 IF sqlca.sqlcode = -1 THEN
	F_errorBaseDatos(sqlca,"Lectura de Tabla Agronomos")
	Return False
 ELSEIF sqlca.sqlcode = 100 THEN
	   MessageBox("Atención","No Existe el Código de Agronomo.Ingrese o Seleccione otro.")
		RETURN False
 END IF		
	
RETURN TRUE
end function

on w_info_bisemanal.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.st_6=create st_6
this.st_1=create st_1
this.uo_seltemporada=create uo_seltemporada
this.em_semana=create em_semana
this.em_fecha=create em_fecha
this.st_7=create st_7
this.rb_spacking=create rb_spacking
this.rb_cpacking=create rb_cpacking
this.st_9=create st_9
this.dw_1=create dw_1
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.uo_seltemporada
this.Control[iCurrent+7]=this.em_semana
this.Control[iCurrent+8]=this.em_fecha
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.rb_spacking
this.Control[iCurrent+11]=this.rb_cpacking
this.Control[iCurrent+12]=this.st_9
this.Control[iCurrent+13]=this.dw_1
this.Control[iCurrent+14]=this.uo_selespecie
this.Control[iCurrent+15]=this.uo_selvariedad
end on

on w_info_bisemanal.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.st_1)
destroy(this.uo_seltemporada)
destroy(this.em_semana)
destroy(this.em_fecha)
destroy(this.st_7)
destroy(this.rb_spacking)
destroy(this.rb_cpacking)
destroy(this.st_9)
destroy(this.dw_1)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelTemporada.codigo)Then lb_Cerrar	=	True
If IsNull(uo_SelEspecie.codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelTemporada.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	
	iuo_semana	=	Create uo_nrosemana
	
	uo_SelTemporada.Inicia(gstr_tempo.Temporada)
	uo_SelEspecie.Inicia(11)
	uo_SelVariedad.Filtra(11)
End If

end event

type pb_excel from w_para_informes`pb_excel within w_info_bisemanal
integer x = 2021
integer y = 388
end type

event pb_excel::clicked;call super::clicked;Long	fila
String	ls_Archivo

If em_semana.Text = '' Then 
	MessageBox('Atención', 'Debe seleccionar una semana.')
	Return 
End If

dw_1.SetTransObject(sqlca)

fila = dw_1.Retrieve(uo_SelTemporada.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, Integer(em_semana.Text), Date(em_Fecha.Text))

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Archivo)

ls_Archivo += '\PentaSemanal_E' + String(uo_SelEspecie.Codigo, '00') + 'S' + em_semana.Text + '.xls'

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Archivo, Excel!, True) = 1 Then
		MessageBox('Atención', 'Archivo Generado Exitosamente.', Information!, Ok!)
	Else
		MessageBox('Atención', 'No se pudo generar archivo.', Information!, Ok!)
	End If
End If
end event

type st_computador from w_para_informes`st_computador within w_info_bisemanal
end type

type st_usuario from w_para_informes`st_usuario within w_info_bisemanal
end type

type st_temporada from w_para_informes`st_temporada within w_info_bisemanal
end type

type p_logo from w_para_informes`p_logo within w_info_bisemanal
end type

type st_titulo from w_para_informes`st_titulo within w_info_bisemanal
integer x = 219
integer width = 1627
string text = "Pronostico Penta Semanal"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_bisemanal
integer x = 2007
integer y = 640
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Long		fila, ll_fila

istr_info.titulo	= "PRONOSTICO PENTASEMANAL"
istr_info.copias	= 1

If em_semana.Text = '' Then 
	MessageBox('Atención', 'Debe seleccionar una semana.')
	Return 
End If

OpenWithParm(vinf, istr_info)

If rb_spacking.Checked Then
	vinf.dw_1.DataObject = "dw_info_pentasemanal"
	vinf.dw_1.Modify('DataWindow.Zoom = 70')
Else
	vinf.dw_1.DataObject = "dw_info_pentasemanal_packing"
	vinf.dw_1.Modify('DataWindow.Zoom = 63')
End If

vinf.dw_1.SetTransObject(sqlca)

ll_Fila = vinf.dw_1.Retrieve(uo_SelTemporada.Codigo, -1, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, Integer(em_semana.Text), Date(em_Fecha.Text))

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_bisemanal
integer x = 1998
integer y = 888
integer taborder = 60
end type

type st_2 from statictext within w_info_bisemanal
integer x = 338
integer y = 648
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_bisemanal
integer x = 338
integer y = 520
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Temporada"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_bisemanal
integer x = 891
integer y = 1072
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lunes Sem"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_bisemanal
integer x = 338
integer y = 900
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_bisemanal
integer x = 219
integer y = 1192
integer width = 1627
integer height = 224
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_seltemporada from uo_seleccion_paramtemporada within w_info_bisemanal
integer x = 731
integer y = 512
integer height = 76
integer taborder = 20
boolean bringtotop = true
end type

on uo_seltemporada.destroy
call uo_seleccion_paramtemporada::destroy
end on

type em_semana from editmask within w_info_bisemanal
integer x = 731
integer y = 1060
integer width = 133
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "00"
end type

event modified;Integer	li_Semana

If (Integer(This.Text) > 53) Or (Integer(This.Text) < 1) Then
	MessageBox('Atención', 'Semana debe estar comprendida entre 1 y 53')
	This.Text = ''
	Return -1
Else
	If iuo_Semana.Semana(Integer(This.Text), uo_SelTemporada.Codigo, uo_SelEspecie.Codigo) Then
		li_Semana	=	F_NroSemanaAno(iuo_Semana.Lunes)
		If Integer(This.Text) = 53 Then
			If 	li_Semana = 52 Then
				This.Text = ''
				MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
				Return -1
			End If
		End If
		em_Fecha.Text = String(iuo_Semana.Lunes, 'dd/mm/yyyy')
	End If
End If
end event

type em_fecha from editmask within w_info_bisemanal
integer x = 1257
integer y = 1060
integer width = 448
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
end type

type st_7 from statictext within w_info_bisemanal
integer x = 338
integer y = 1072
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Semana"
boolean focusrectangle = false
end type

type rb_spacking from radiobutton within w_info_bisemanal
integer x = 544
integer y = 1260
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "&Sin Packing"
boolean checked = true
end type

type rb_cpacking from radiobutton within w_info_bisemanal
integer x = 1111
integer y = 1260
integer width = 489
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "&Con Packing"
end type

type st_9 from statictext within w_info_bisemanal
integer x = 219
integer y = 440
integer width = 1627
integer height = 752
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_info_bisemanal
boolean visible = false
integer x = 1966
integer y = 1132
integer width = 219
integer height = 152
integer taborder = 30
boolean bringtotop = true
string dataobject = "dw_gene_pentasemanal_excel"
boolean vscrollbar = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_bisemanal
event destroy ( )
integer x = 731
integer y = 644
integer height = 76
integer taborder = 70
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
End Choose
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_bisemanal
event destroy ( )
integer x = 731
integer y = 804
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

