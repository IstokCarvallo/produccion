$PBExportHeader$w_info_embarque_prodvarzonasem.srw
forward
global type w_info_embarque_prodvarzonasem from w_para_informes
end type
type st_4 from statictext within w_info_embarque_prodvarzonasem
end type
type st_6 from statictext within w_info_embarque_prodvarzonasem
end type
type st_3 from statictext within w_info_embarque_prodvarzonasem
end type
type st_13 from statictext within w_info_embarque_prodvarzonasem
end type
type em_semana from editmask within w_info_embarque_prodvarzonasem
end type
type st_14 from statictext within w_info_embarque_prodvarzonasem
end type
type em_ano from editmask within w_info_embarque_prodvarzonasem
end type
type st_7 from statictext within w_info_embarque_prodvarzonasem
end type
type st_10 from statictext within w_info_embarque_prodvarzonasem
end type
type dw_recibidor from datawindow within w_info_embarque_prodvarzonasem
end type
type cbx_recibidor from checkbox within w_info_embarque_prodvarzonasem
end type
type cbx_recibidorcons from checkbox within w_info_embarque_prodvarzonasem
end type
type st_1 from statictext within w_info_embarque_prodvarzonasem
end type
type cbx_planta from checkbox within w_info_embarque_prodvarzonasem
end type
type cbx_plantascons from checkbox within w_info_embarque_prodvarzonasem
end type
type dw_planta from datawindow within w_info_embarque_prodvarzonasem
end type
type st_8 from statictext within w_info_embarque_prodvarzonasem
end type
type cbx_peso from checkbox within w_info_embarque_prodvarzonasem
end type
type tit_peso from statictext within w_info_embarque_prodvarzonasem
end type
type dw_pesoneto from datawindow within w_info_embarque_prodvarzonasem
end type
type gb_3 from groupbox within w_info_embarque_prodvarzonasem
end type
type st_5 from statictext within w_info_embarque_prodvarzonasem
end type
type uo_selespecie from uo_seleccion_especie within w_info_embarque_prodvarzonasem
end type
type cbx_varirotula from checkbox within w_info_embarque_prodvarzonasem
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_embarque_prodvarzonasem
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_embarque_prodvarzonasem
end type
end forward

global type w_info_embarque_prodvarzonasem from w_para_informes
integer x = 14
integer y = 32
integer width = 3611
integer height = 2172
string title = "Embarque Productor/Variedad/Zonas/Semanas"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_6 st_6
st_3 st_3
st_13 st_13
em_semana em_semana
st_14 st_14
em_ano em_ano
st_7 st_7
st_10 st_10
dw_recibidor dw_recibidor
cbx_recibidor cbx_recibidor
cbx_recibidorcons cbx_recibidorcons
st_1 st_1
cbx_planta cbx_planta
cbx_plantascons cbx_plantascons
dw_planta dw_planta
st_8 st_8
cbx_peso cbx_peso
tit_peso tit_peso
dw_pesoneto dw_pesoneto
gb_3 gb_3
st_5 st_5
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
uo_selproductor uo_selproductor
uo_selcliente uo_selcliente
end type
global w_info_embarque_prodvarzonasem w_info_embarque_prodvarzonasem

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_planta, idwc_pesoneto, idwc_recibidor


end variables

on w_info_embarque_prodvarzonasem.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_6=create st_6
this.st_3=create st_3
this.st_13=create st_13
this.em_semana=create em_semana
this.st_14=create st_14
this.em_ano=create em_ano
this.st_7=create st_7
this.st_10=create st_10
this.dw_recibidor=create dw_recibidor
this.cbx_recibidor=create cbx_recibidor
this.cbx_recibidorcons=create cbx_recibidorcons
this.st_1=create st_1
this.cbx_planta=create cbx_planta
this.cbx_plantascons=create cbx_plantascons
this.dw_planta=create dw_planta
this.st_8=create st_8
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.dw_pesoneto=create dw_pesoneto
this.gb_3=create gb_3
this.st_5=create st_5
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.uo_selproductor=create uo_selproductor
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_13
this.Control[iCurrent+5]=this.em_semana
this.Control[iCurrent+6]=this.st_14
this.Control[iCurrent+7]=this.em_ano
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.st_10
this.Control[iCurrent+10]=this.dw_recibidor
this.Control[iCurrent+11]=this.cbx_recibidor
this.Control[iCurrent+12]=this.cbx_recibidorcons
this.Control[iCurrent+13]=this.st_1
this.Control[iCurrent+14]=this.cbx_planta
this.Control[iCurrent+15]=this.cbx_plantascons
this.Control[iCurrent+16]=this.dw_planta
this.Control[iCurrent+17]=this.st_8
this.Control[iCurrent+18]=this.cbx_peso
this.Control[iCurrent+19]=this.tit_peso
this.Control[iCurrent+20]=this.dw_pesoneto
this.Control[iCurrent+21]=this.gb_3
this.Control[iCurrent+22]=this.st_5
this.Control[iCurrent+23]=this.uo_selespecie
this.Control[iCurrent+24]=this.cbx_varirotula
this.Control[iCurrent+25]=this.uo_selproductor
this.Control[iCurrent+26]=this.uo_selcliente
end on

on w_info_embarque_prodvarzonasem.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_13)
destroy(this.em_semana)
destroy(this.st_14)
destroy(this.em_ano)
destroy(this.st_7)
destroy(this.st_10)
destroy(this.dw_recibidor)
destroy(this.cbx_recibidor)
destroy(this.cbx_recibidorcons)
destroy(this.st_1)
destroy(this.cbx_planta)
destroy(this.cbx_plantascons)
destroy(this.dw_planta)
destroy(this.st_8)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.dw_pesoneto)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.uo_selproductor)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean lb_cerrar

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecie.Seleccion(True, True)
	uo_SelProductor.Seleccion(True, True)
	uo_SelCliente.Seleccion(False, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1)
	dw_planta.InsertRow(0)
	
	dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
	idwc_recibidor.SetTransObject(sqlca)
	idwc_recibidor.Retrieve()
	dw_recibidor.InsertRow(0)
	
	dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
	idwc_pesoneto.SetTransObject(SQLCA)
	idwc_pesoneto.Retrieve()
	dw_pesoneto.InsertRow(0)
	dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.ModIfy("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
	
	em_semana.Text				=	"45"
	em_ano.Text					=	String(year(gd_TempoInicio))
	istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
	istr_mant.argumento[2]	= 	"0"							//	planta
	//istr_mant.argumento[5]	=	String(gi_CodEspecie)	//	especie
	istr_mant.argumento[6]  =  "0"							//	productor
	istr_mant.argumento[8]  =  "1"							//	peso
	istr_mant.argumento[9]  =  em_semana.text				//	semana inicial
	istr_mant.argumento[10] =  em_ano.text					//	año inicio temporada
	istr_mant.argumento[11]	=	"1"							// Consolida Productor 1 = SI
	istr_mant.argumento[12]	=	"1"							// Consolida Plantas   1 = Si	
	istr_mant.argumento[13]	=	"0"							// recibidor	
	istr_mant.argumento[14]	=	"1"							// Consolida recibidor 1 = Si
	istr_mant.argumento[15]	=	"0"							// Mercado
	istr_mant.argumento[16]	=	"0"							// Consolida zonas     1 = Si
	istr_mant.argumento[17]	=	"0"							// zonas

End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_embarque_prodvarzonasem
end type

type st_computador from w_para_informes`st_computador within w_info_embarque_prodvarzonasem
end type

type st_usuario from w_para_informes`st_usuario within w_info_embarque_prodvarzonasem
end type

type st_temporada from w_para_informes`st_temporada within w_info_embarque_prodvarzonasem
end type

type p_logo from w_para_informes`p_logo within w_info_embarque_prodvarzonasem
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_embarque_prodvarzonasem
integer width = 1847
string text = "Embarque Productor/Variedad/Zonas/Semanas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_embarque_prodvarzonasem
integer x = 2277
integer y = 1404
integer taborder = 160
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	ll_Fila, li_planta, li_varirotula, li_semana, li_NroSemana, &
			li_tipo = 1,li_semanatope
Long		ll_semana_ano, ll_productor, ll_recibidor,ll_semanatope
Date		ld_desde, ld_hasta, ld_FechaInicio, ld_Fecha,ld_FechaPrincipal
String		ls_cajas, ls_planta, ls_productor, ls_encabezado, ls_recibidor,ls_especie, ls_lista

istr_info.titulo	= 'EMBARQUES POR PRODUCTOR/VARIEDAD/ZONAS/SEMANAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_embarque_semanal_01"

li_planta		=	Integer(istr_mant.argumento[2])
ll_productor	=	Long(istr_mant.argumento[6])
ll_recibidor	=	Long(istr_mant.argumento[13])
li_semana		=	Integer(istr_mant.argumento[9])
ll_semana_ano	=	Integer(istr_mant.argumento[10]) * 100 + li_Semana

If cbx_peso.Checked=False Then
	ls_cajas = "Bulto"
	istr_mant.argumento[8]	=	"1"
Else
	istr_mant.argumento[8]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas = "Base " + istr_mant.argumento[8] 
End If

/*
Especies
*/
If IsNull(uo_selespecie.Codigo)Then
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
End If

If cbx_varirotula.Checked Then
	li_varirotula = 1
Else
	li_varirotula = 0
End If

DECLARE InicioSemana PROCEDURE FOR dbo.FProc_InicioSemana
		@semana 		= 	:ll_Semana_Ano,
		@tipo   		= 	:li_tipo  ;
EXECUTE InicioSemana;

If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Asignacion Inicio de Semana")
	RETURN
ElseIf SQLCA.SQLCode = 0 Then
	
	FETCH InicioSemana INTO :ld_FechaInicio ;
	
	If SQLCA.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca,"Asignacion Inicio de Semana")
	Else
		CLOSE InicioSemana	;
	End If
End If

ld_Fecha	=	ld_FechaInicio

If cbx_plantascons.checked Then
	ls_planta = 'Consolidadas'
Else
	If cbx_planta.checked Then
		ls_planta = 'Todas'
	Else
		SELECT plde_nombre INTO:ls_planta
		FROM dbo.plantadesp
		WHERE plde_codigo=:li_planta;
		ls_planta = String(li_planta,'00')+" "+ls_planta
	End If
End If

/*
productor
*/
ls_lista = uo_selproductor.Lista

If ls_lista = '-9' Then
	ls_productor = 'Consolidados'
Else
	If ls_lista = '-1' Then
		ls_productor = 'Todos'
	Else
		SELECT prod_nombre INTO:ls_productor
		FROM dbo.productores
		WHERE prod_codigo=:ls_lista;
		ls_productor = String(ls_lista,'00000')+" "+ls_productor
		
		If ls_productor = '00000' Then
			ls_productor = ls_lista
		End If	
		
	End If
End If

If cbx_recibidorcons.checked Then
	ls_recibidor = 'Consolidados'
Else
	If cbx_recibidor.checked Then
		ls_recibidor = 'Todos'
	Else
		SELECT cons_nombre INTO:ls_recibidor
		FROM dbo.consignatario
		WHERE cons_codigo=:ll_recibidor;
		ls_recibidor = String(ll_recibidor,'00000')+" "+ls_recibidor
	End If
End If

ls_especie = String(uo_selespecie.Codigo,'00')+" "+uo_selespecie.Nombre


ld_FechaPrincipal	=	gd_fecultsemana//Date(em_ano.Text+'-12-31')	//

SELECT dbo.F_Semana(:ld_FechaPrincipal, 1) 
INTO :ll_semanatope
FROM dbo.parempresa;
li_semanatope	=	Integer(Right(String(ll_semanatope),2))

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, li_planta, uo_selespecie.Codigo,&
										 Dec(istr_mant.argumento[8]),integer(istr_mant.argumento[14]),&
										 Integer(istr_mant.argumento[12]),li_semana,ll_semana_ano,&
										 Long(istr_mant.argumento[13]),Integer(istr_mant.argumento[15]),&
										 Integer(istr_mant.argumento[16]),Integer(istr_mant.argumento[17]),&
										 li_varirotula,ls_lista)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	   F_Membrete(vinf.dw_1)
		FOR li_NroSemana = 1 TO li_semanatope
			If li_NroSemana < 27 Then
				ls_encabezado	=  "Semana" + String(li_NroSemana, '00') + "_t.Text = '" + &
										String(ld_Fecha, 'dd/mm/yyyy') + "~n~r" + &
										String(li_Semana, '00') + "'"
				vinf.dw_1.ModIfy(ls_encabezado)
			Else
				ls_encabezado	=	"Semana" + String(li_NroSemana, '00') + "_t.Text = '" + &
										String(li_Semana, '00') + "~n~r" + &
										String(ld_Fecha, 'dd/mm/yyyy') + "'"
				vinf.dw_1.ModIfy(ls_encabezado)
			End If
			
			ld_Fecha		=	RelativeDate(ld_Fecha, 7)
			li_Semana	++
			
			If li_Semana = li_semanatope + 1 Then
				li_Semana	=	1	
			End If
		NEXT
		vinf.dw_1.ModIfy("base.text = '" + ls_cajas + "'")
		vinf.dw_1.ModIfy("tit_especie.text = '" + ls_especie + "'")
		vinf.dw_1.ModIfy("tit_productor.text = '" + ls_productor + "'")
		vinf.dw_1.ModIfy("tit_planta.text = '" + ls_planta + "'")
		vinf.dw_1.ModIfy("tit_recibidor.text = '" + ls_recibidor + "'")
		
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_embarque_prodvarzonasem
integer x = 2277
integer y = 1692
integer taborder = 170
end type

type st_4 from statictext within w_info_embarque_prodvarzonasem
integer x = 247
integer y = 440
integer width = 1847
integer height = 764
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_embarque_prodvarzonasem
integer x = 343
integer y = 468
integer width = 233
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_embarque_prodvarzonasem
integer x = 343
integer y = 1324
integer width = 421
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
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_embarque_prodvarzonasem
integer x = 343
integer y = 1716
integer width = 434
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
string text = "Semana Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_semana from editmask within w_info_embarque_prodvarzonasem
integer x = 1285
integer y = 1716
integer width = 251
integer height = 100
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "45"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "00"
boolean spin = true
string displaydata = "~t/"
double increment = 1
string minmax = "1~~52"
end type

event modified;istr_mant.argumento[9]  =  This.text	
end event

type st_14 from statictext within w_info_embarque_prodvarzonasem
integer x = 352
integer y = 1832
integer width = 773
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
string text = "Año Inicio de Temporada"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_ano from editmask within w_info_embarque_prodvarzonasem
integer x = 1280
integer y = 1832
integer width = 334
integer height = 100
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "0000"
boolean spin = true
string displaydata = "~t/"
double increment = 1
string minmax = "~~"
end type

event em_ano::modified;call super::modified;istr_mant.argumento[10]  =  This.text	
end event

type st_7 from statictext within w_info_embarque_prodvarzonasem
integer x = 247
integer y = 1688
integer width = 1847
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_embarque_prodvarzonasem
integer x = 343
integer y = 1088
integer width = 416
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
string text = "Consignatario"
boolean focusrectangle = false
end type

type dw_recibidor from datawindow within w_info_embarque_prodvarzonasem
integer x = 786
integer y = 1092
integer width = 1125
integer height = 104
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_consignatarios"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[13]	=	data
istr_mant.argumento[14]	=	'0'

end event

event itemerror;RETURN 1
end event

type cbx_recibidor from checkbox within w_info_embarque_prodvarzonasem
integer x = 786
integer y = 1020
integer width = 402
integer height = 80
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_recibidorcons.Enabled									=	True
	dw_recibidor.Enabled											=	False
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[13]									=	'0'
	istr_mant.argumento[14]									=	'0'
ELSE
	cbx_recibidorcons.Enabled									=	False
	cbx_recibidorcons.Checked									=	False
	dw_recibidor.Enabled											=	True
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_recibidor.SetFocus()
END IF

	
end event

type cbx_recibidorcons from checkbox within w_info_embarque_prodvarzonasem
integer x = 1262
integer y = 1020
integer width = 471
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[14]	=	'1'
ELSE
	istr_mant.argumento[14]	=	'0'
END IF
	
end event

type st_1 from statictext within w_info_embarque_prodvarzonasem
integer x = 343
integer y = 628
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_embarque_prodvarzonasem
integer x = 786
integer y = 556
integer width = 402
integer height = 76
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_plantascons.Enabled									=	True
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[2]									=	'0'
	istr_mant.argumento[12]									=	'0'
ELSE
	cbx_plantascons.Enabled									=	False
	cbx_plantascons.Checked									=	False
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_planta.SetFocus()
END IF
end event

type cbx_plantascons from checkbox within w_info_embarque_prodvarzonasem
integer x = 1262
integer y = 556
integer width = 471
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[12]	=	'1'
ELSE
	istr_mant.argumento[12]	=	'0'
END IF
	
end event

type dw_planta from datawindow within w_info_embarque_prodvarzonasem
integer x = 786
integer y = 632
integer width = 969
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
	istr_mant.argumento[12]	=  "0"
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_embarque_prodvarzonasem
integer x = 343
integer y = 900
integer width = 306
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
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_embarque_prodvarzonasem
integer x = 384
integer y = 1536
integer width = 631
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
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type tit_peso from statictext within w_info_embarque_prodvarzonasem
integer x = 1111
integer y = 1544
integer width = 160
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
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_pesoneto from datawindow within w_info_embarque_prodvarzonasem
integer x = 1280
integer y = 1532
integer width = 695
integer height = 92
integer taborder = 130
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type gb_3 from groupbox within w_info_embarque_prodvarzonasem
integer x = 320
integer y = 1464
integer width = 1705
integer height = 200
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_embarque_prodvarzonasem
integer x = 247
integer y = 1208
integer width = 1847
integer height = 480
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_embarque_prodvarzonasem
event destroy ( )
integer x = 777
integer y = 1216
integer height = 180
integer taborder = 110
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type cbx_varirotula from checkbox within w_info_embarque_prodvarzonasem
integer x = 777
integer y = 1412
integer width = 745
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
string text = "Variedad Rotulada"
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_embarque_prodvarzonasem
integer x = 791
integer y = 736
integer taborder = 160
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;
uo_SelProductor.Filtra(-1,-1, uo_SelCliente.Codigo)
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_embarque_prodvarzonasem
integer x = 786
integer y = 464
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 
	
Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_selproductor.Filtra(-1,-1, This.Codigo)
End Choose
end event

