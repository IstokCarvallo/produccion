$PBExportHeader$w_info_guia_despacho_muestras.srw
forward
global type w_info_guia_despacho_muestras from w_para_informes
end type
type st_1 from statictext within w_info_guia_despacho_muestras
end type
type st_2 from statictext within w_info_guia_despacho_muestras
end type
type st_6 from statictext within w_info_guia_despacho_muestras
end type
type em_nroguia from editmask within w_info_guia_despacho_muestras
end type
type st_3 from statictext within w_info_guia_despacho_muestras
end type
type st_5 from statictext within w_info_guia_despacho_muestras
end type
type st_7 from statictext within w_info_guia_despacho_muestras
end type
type st_8 from statictext within w_info_guia_despacho_muestras
end type
type em_recibidor from editmask within w_info_guia_despacho_muestras
end type
type em_rut from editmask within w_info_guia_despacho_muestras
end type
type em_direccion from editmask within w_info_guia_despacho_muestras
end type
type st_9 from statictext within w_info_guia_despacho_muestras
end type
type em_observa from editmask within w_info_guia_despacho_muestras
end type
type st_4 from statictext within w_info_guia_despacho_muestras
end type
type em_ciudad from editmask within w_info_guia_despacho_muestras
end type
type st_10 from statictext within w_info_guia_despacho_muestras
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_guia_despacho_muestras
end type
type uo_selplanta from uo_seleccion_plantas within w_info_guia_despacho_muestras
end type
end forward

global type w_info_guia_despacho_muestras from w_para_informes
integer x = 14
integer y = 32
integer width = 3250
integer height = 1592
string title = "Módulo de Despacho"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
st_6 st_6
em_nroguia em_nroguia
st_3 st_3
st_5 st_5
st_7 st_7
st_8 st_8
em_recibidor em_recibidor
em_rut em_rut
em_direccion em_direccion
st_9 st_9
em_observa em_observa
st_4 st_4
em_ciudad em_ciudad
st_10 st_10
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_guia_despacho_muestras w_info_guia_despacho_muestras

type variables
uo_GuiaDespacho		iuo_Guia
uo_Despachos			iuo_Despacho
end variables

forward prototypes
public function boolean existeguia (long al_guia)
end prototypes

public function boolean existeguia (long al_guia);Integer	li_tecnic, sire_codigo
Date		ld_fecha
String		ls_Embarque, ls_ciudad, ls_direccion, ls_rut, ls_nombre, ls_Glosa

iuo_Despacho.of_ExisteGuia(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, al_guia, 32, False, SQLCA) 

IF al_guia <> 0 Then	
	SELECT embq_codigo, defe_tecnic,sire_codigo, defe_glosas
		INTO	:ls_Embarque, :li_tecnic,:sire_codigo, :ls_Glosa
		FROM	dbo.DESPAFRIGOEN
		WHERE	plde_codigo =	:uo_SelPlanta.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_guides	=	:al_guia
		AND	defe_tiposa =	32
		Using SQLCA;
	
	SELECT sit.sire_nombre,ciu.ciud_nombre,sire_direcc,sit.sire_codrut 
		INTO	:ls_nombre,:ls_ciudad,:ls_direccion,:ls_rut  
		FROM dbo.sitiorevision as sit,dbo.ciudad as ciu,dbo.provincias as pro  
		WHERE ciu.ciud_codigo = sit.ciud_codigo and  
			ciu.regi_codigo = sit.regi_codigo and  
			ciu.prov_codigo = sit.prov_codigo and  
			ciu.regi_codigo = pro.regi_codigo and  
			ciu.prov_codigo = pro.prov_codigo and
			sit.sire_codigo = :sire_codigo
			Using SQLCA;

	IF sqlca.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Despachos")
		em_nroguia.SetFocus()
		Return False
	ElseIF sqlca.SQLCode = 100 Then
			MessageBox("Atención", "No existe Embarque.~r~r", Exclamation!, Ok!)
			pb_acepta.Enabled	= False
			em_nroguia.SetFocus()
			Return False
	Else
		IF ls_nombre <> '' Then
			em_recibidor.Text = ls_nombre
			em_rut.Text = ls_rut
			em_direccion.Text = ls_direccion
			em_ciudad.Text = ls_ciudad
			em_observa.Text = ls_Glosa
			pb_acepta.Enabled = True
		End IF	
	End IF
End IF		
end function

on w_info_guia_despacho_muestras.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_6=create st_6
this.em_nroguia=create em_nroguia
this.st_3=create st_3
this.st_5=create st_5
this.st_7=create st_7
this.st_8=create st_8
this.em_recibidor=create em_recibidor
this.em_rut=create em_rut
this.em_direccion=create em_direccion
this.st_9=create st_9
this.em_observa=create em_observa
this.st_4=create st_4
this.em_ciudad=create em_ciudad
this.st_10=create st_10
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.em_nroguia
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.st_7
this.Control[iCurrent+8]=this.st_8
this.Control[iCurrent+9]=this.em_recibidor
this.Control[iCurrent+10]=this.em_rut
this.Control[iCurrent+11]=this.em_direccion
this.Control[iCurrent+12]=this.st_9
this.Control[iCurrent+13]=this.em_observa
this.Control[iCurrent+14]=this.st_4
this.Control[iCurrent+15]=this.em_ciudad
this.Control[iCurrent+16]=this.st_10
this.Control[iCurrent+17]=this.uo_selcliente
this.Control[iCurrent+18]=this.uo_selplanta
end on

on w_info_guia_despacho_muestras.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.em_nroguia)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.em_recibidor)
destroy(this.em_rut)
destroy(this.em_direccion)
destroy(this.st_9)
destroy(this.em_observa)
destroy(this.st_4)
destroy(this.em_ciudad)
destroy(this.st_10)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	iuo_Guia			=	Create uo_GuiaDespacho
	iuo_Despacho	=	Create uo_Despachos
	
	st_titulo.Text		 	=	'Emisión Guia de Despacho Muestra Lotes Inspección'
End If

end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_guia_despacho_muestras
integer x = 2715
integer y = 1244
end type

type st_computador from w_para_informes`st_computador within w_info_guia_despacho_muestras
end type

type st_usuario from w_para_informes`st_usuario within w_info_guia_despacho_muestras
end type

type st_temporada from w_para_informes`st_temporada within w_info_guia_despacho_muestras
end type

type p_logo from w_para_informes`p_logo within w_info_guia_despacho_muestras
end type

type st_titulo from w_para_informes`st_titulo within w_info_guia_despacho_muestras
integer width = 2469
string text = "Emisión Guia de Despacho Muestras Lote Inspección"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_guia_despacho_muestras
integer x = 2885
integer y = 692
integer taborder = 100
end type

event pb_acepta::clicked;call super::clicked;SetPointer(Arrow!)

Integer	fila,  li_EmisionGuia
Long		ll_nroguia, ll_Fila, li_control = 0, li_tipoembq

istr_info.titulo	= 'EMISION GUIA DE DESPACHO'
OpenWithParm(vinf, istr_info)

ll_NroGuia	=	Long(em_nroguia.Text)

If IsNull(uo_SelCliente.FormatoGuia) OR uo_SelCliente.FormatoGuia = '' Then
	vinf.dw_1.DataObject = 'dw_info_guia_despacho_cal'
Else
	vinf.dw_1.DataObject	= uo_SelCliente.FormatoGuia
End If

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_nroguia,1,1, 1, 1,1,1,1, li_control, iuo_Despacho.Despacho)	

/*Selecciona tipo se salida del embarque*/
	SELECT defe_tiposa, IsNull(defe_guiaem, 0)
		INTO	:li_tipoembq, :li_EmisionGuia
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:uo_SelPlanta.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_guides	=	:ll_nroguia
		And	defe_tiposa = 32;

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
//		
//		Emite Guia Despacho
//			
	iuo_Guia.of_SetGlosaGD(em_observa.Text)
	
	If li_EmisionGuia = 3 Or li_EmisionGuia = 0 Then 
		If iuo_Guia.of_EmiteGuia(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_NroGuia, False,1,1,1,1,1,1,0, 0, iuo_Despacho.Despacho)  > 0 Then
			If Not iuo_Guia.of_GeneraLibroGuia(0) Then 
				MessageBox('Alerta', 'No se pudo actualziar Libro de guias de despacho.', Information!, OK!)
			End If
			iuo_Guia.of_ActualizaEstadoGD(1, uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_NroGuia, 1, SQLCA)
			iuo_Guia.of_RecuperaPDF(ll_NroGuia,vinf.dw_1.Object.defe_fecdes[1], 2)
		End If
	Else
		iuo_Guia.of_RecuperaPDF(ll_NroGuia, vinf.dw_1.Object.defe_fecdes[1], 2)
	End If
	
	iuo_Despacho.of_ActualizaEstado(1, uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_NroGuia, SQLCA)

	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	pb_acepta.Enabled = False
End If

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_guia_despacho_muestras
integer x = 2889
integer y = 972
integer taborder = 110
end type

type st_1 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 544
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

type st_2 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 652
integer width = 325
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
string text = "Nro. Guia"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 448
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

type em_nroguia from editmask within w_info_guia_despacho_muestras
integer x = 905
integer y = 644
integer width = 402
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF ExisteGuia(Long(This.Text)) = False THEN
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_info_guia_despacho_muestras
integer x = 251
integer y = 772
integer width = 2469
integer height = 512
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 804
integer width = 402
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
string text = "Despacho a"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 896
integer width = 402
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
string text = "Rut"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 988
integer width = 402
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
string text = "Dirección"
boolean focusrectangle = false
end type

type em_recibidor from editmask within w_info_guia_despacho_muestras
integer x = 905
integer y = 788
integer width = 1765
integer height = 88
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type em_rut from editmask within w_info_guia_despacho_muestras
integer x = 905
integer y = 880
integer width = 480
integer height = 88
integer taborder = 60
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
maskdatatype maskdatatype = stringmask!
string mask = "###.###.###-a"
end type

type em_direccion from editmask within w_info_guia_despacho_muestras
integer x = 905
integer y = 972
integer width = 1765
integer height = 88
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_9 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 1172
integer width = 581
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
string text = "Observaciones G.D."
boolean focusrectangle = false
end type

type em_observa from editmask within w_info_guia_despacho_muestras
integer x = 905
integer y = 1156
integer width = 1765
integer height = 88
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
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string minmax = "~~200"
end type

type st_4 from statictext within w_info_guia_despacho_muestras
integer x = 251
integer y = 420
integer width = 2469
integer height = 352
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

type em_ciudad from editmask within w_info_guia_despacho_muestras
integer x = 905
integer y = 1064
integer width = 1765
integer height = 88
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
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_10 from statictext within w_info_guia_despacho_muestras
integer x = 288
integer y = 1080
integer width = 402
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
string text = "Ciudad"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_guia_despacho_muestras
integer x = 901
integer y = 440
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_guia_despacho_muestras
event destroy ( )
integer x = 901
integer y = 540
integer width = 965
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

