$PBExportHeader$w_reimprecion_de_compactos.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_reimprecion_de_compactos from w_para_informes
end type
type st_1 from statictext within w_reimprecion_de_compactos
end type
type dw_cliente from datawindow within w_reimprecion_de_compactos
end type
type st_2 from statictext within w_reimprecion_de_compactos
end type
type dw_plantadesp from datawindow within w_reimprecion_de_compactos
end type
type em_numero from editmask within w_reimprecion_de_compactos
end type
type st_3 from statictext within w_reimprecion_de_compactos
end type
type st_5 from statictext within w_reimprecion_de_compactos
end type
type dw_16 from datawindow within w_reimprecion_de_compactos
end type
type rb_1 from radiobutton within w_reimprecion_de_compactos
end type
type rb_2 from radiobutton within w_reimprecion_de_compactos
end type
type em_1 from editmask within w_reimprecion_de_compactos
end type
type st_4 from statictext within w_reimprecion_de_compactos
end type
type st_6 from statictext within w_reimprecion_de_compactos
end type
type em_camara from editmask within w_reimprecion_de_compactos
end type
type st_7 from statictext within w_reimprecion_de_compactos
end type
type dw_mercado from datawindow within w_reimprecion_de_compactos
end type
type ddlb_sdp from dropdownlistbox within w_reimprecion_de_compactos
end type
type dw_1 from datawindow within w_reimprecion_de_compactos
end type
end forward

global type w_reimprecion_de_compactos from w_para_informes
integer width = 3273
integer height = 1552
string title = "REIMPRESION DE COMPACTOS"
boolean minbox = false
event ue_validapassword ( )
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
em_numero em_numero
st_3 st_3
st_5 st_5
dw_16 dw_16
rb_1 rb_1
rb_2 rb_2
em_1 em_1
st_4 st_4
st_6 st_6
em_camara em_camara
st_7 st_7
dw_mercado dw_mercado
ddlb_sdp ddlb_sdp
dw_1 dw_1
end type
global w_reimprecion_de_compactos w_reimprecion_de_compactos

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas, idwc_mercado
Integer	ii_planta, ii_cliente, ii_sdp
Long		il_NroPallet, il_NroCaja
String	is_Computador
uo_lotescorrelequipo_gr	iuo_correl
uo_voicecode				iuo_voicecode
uo_QR						iuo_QR

end variables

forward prototypes
public function boolean wf_actualiza_db ()
end prototypes

event ue_validapassword();Str_mant		lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean wf_actualiza_db ();return True
end function

on w_reimprecion_de_compactos.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.em_numero=create em_numero
this.st_3=create st_3
this.st_5=create st_5
this.dw_16=create dw_16
this.rb_1=create rb_1
this.rb_2=create rb_2
this.em_1=create em_1
this.st_4=create st_4
this.st_6=create st_6
this.em_camara=create em_camara
this.st_7=create st_7
this.dw_mercado=create dw_mercado
this.ddlb_sdp=create ddlb_sdp
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.dw_16
this.Control[iCurrent+9]=this.rb_1
this.Control[iCurrent+10]=this.rb_2
this.Control[iCurrent+11]=this.em_1
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.st_6
this.Control[iCurrent+14]=this.em_camara
this.Control[iCurrent+15]=this.st_7
this.Control[iCurrent+16]=this.dw_mercado
this.Control[iCurrent+17]=this.ddlb_sdp
this.Control[iCurrent+18]=this.dw_1
end on

on w_reimprecion_de_compactos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.dw_16)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.em_1)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.em_camara)
destroy(this.st_7)
destroy(this.dw_mercado)
destroy(this.ddlb_sdp)
destroy(this.dw_1)
end on

event open;call super::open;dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

IF gi_CodExport = 590 THEN
	em_camara.Enabled = True
ELSE	
	em_camara.Enabled = False
END IF	

IF gi_CodExport <> gi_cliebase THEN
	ddlb_sdp.Enabled  = True
ELSE	
	ddlb_sdp.Enabled  = False
END IF

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_mercado.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(SQLCA)
idwc_mercado.Retrieve()
dw_mercado.InsertRow(0)

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)

dw_16.SetTransObject(Sqlca)
dw_1.SetTransObject(Sqlca)

iuo_QR	=	Create uo_QR

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!, is_Computador)

end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_reimprecion_de_compactos
integer x = 2784
integer y = 372
end type

type st_computador from w_para_informes`st_computador within w_reimprecion_de_compactos
end type

type st_usuario from w_para_informes`st_usuario within w_reimprecion_de_compactos
end type

type st_temporada from w_para_informes`st_temporada within w_reimprecion_de_compactos
end type

type p_logo from w_para_informes`p_logo within w_reimprecion_de_compactos
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_reimprecion_de_compactos
integer width = 2354
string text = "Reimpresión de Compactos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_reimprecion_de_compactos
integer x = 2784
integer y = 648
integer taborder = 50
end type

event pb_acepta::clicked;Long		ll_pallet, ll_caja, ll_FilaPallet, ll_NroPallet, ll_Registros, ll_FilaCajas, ll_Fila, ll_cont, ll_Fila1, &
			gl_packing, ll_nrocaja, ll_inicio, ll_final, ll_productor
Integer	li_planta, li_mercado, li_packing, li_resultado, li_formato
String	ls_dw, ls_camara, ls_fecha, ls_gtin_numero, ls_CSG, ls_Codigo, ls_Calibre, ls_Embalaje, ls_QR, ls_Ruta

li_planta = Integer(istr_mant.argumento[2])

iuo_correl	=	Create	uo_lotescorrelequipo_gr

iuo_correl.ExisteCorrel(Integer(istr_mant.argumento[2]), 99, is_Computador, FALSE, sqlca)		

SELECT foad_canocx
INTO :li_formato
FROM dbo.spro_formatosadhesivos
WHERE foad_nofodw = :iuo_correl.loco_dwcomp;

li_mercado = dw_mercado.Object.merc_codigo[1]

If isnull(li_mercado) OR li_mercado = 0 Then 
	MessageBox( "No Existe información", "Falta Ingreso de Mercado.", StopSign!, OK!)
	Return 1						
End If						

If li_mercado = -1 Then Return
dw_16.DataObject =  iuo_correl.loco_dwcomp// ls_dw
dw_16.SetTransObject(Sqlca)

ll_caja = Long(em_1.Text)
If isnull(ll_caja) OR ll_caja = 0 Then ll_caja = -1

ll_pallet = Long(em_numero.Text)
If isnull(ll_pallet) OR ll_pallet = 0 Then
	ll_pallet = -1
End If

ll_cont = dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]),ll_caja)

If ll_cont = 0 Then
	MessageBox( "Atención", "No Existe Información para este Pallet o Caja.", StopSign!, OK!)
	Return 1
End If	

If isnull(dw_1.Object.emba_nrogs1[1])  OR dw_1.Object.emba_nrogs1[1] = '' Then
	li_resultado = MessageBox("Atención", 'Código GS1 NO Existe en Tabla Respectiva, Desea Continuar', Exclamation!, OKCancel!, 2)
	ls_gtin_numero = '00000000000000'
	If li_resultado <> 1 Then Return 1
Else
	ls_gtin_numero =  dw_1.Object.emba_nrogs1[1]
End If

If ll_cont = 0 Then
	MessageBox( "Atención", "No Existe Información para este Pallet o Caja.", StopSign!, OK!)
	Return						
End If	

ls_camara = em_camara.Text

If li_formato = 2 Then
	FOR ll_FilaPallet = 1 TO dw_1.RowCount() STEP 2
							
		ll_NroPallet		=	dw_1.Object.paen_numero[ll_FilaPallet]
		il_NroPallet		=	ll_NroPallet
		il_NroCaja		=	dw_1.Object.pafr_secuen[ll_FilaPallet]
		li_packing		=  dw_1.Object.pafr_copack[ll_FilaPallet]
		
		ll_inicio	=	Long(dw_1.Object.pafr_secuen[ll_FilaPallet])
			
		If ll_FilaPallet + 1 <= dw_1.RowCount() Then
			ll_final		=	Long(dw_1.Object.pafr_secuen[ll_FilaPallet + 1])
		Else
			ll_final		=	Long(dw_1.Object.pafr_secuen[ll_FilaPallet])
		End If
		
		dw_16.Reset()
				
		If integer(istr_mant.argumento[1]) <> gi_Cliebase  Then
			ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),ll_inicio,ll_final,li_mercado,ls_camara,ii_sdp)
				dw_16.Object.calcod[1] =	ls_camara
		Else
			ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),ll_inicio,ll_final,li_mercado)
			If ll_fila = 0 Then
				ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),li_packing,ll_inicio,ll_final,li_mercado)
			End If	
		End If	
	
		If ll_Fila = -1 Then
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
		ElseIf ll_Fila = 0 Then
			MessageBox( "Atención", "No Existe Información para este Pallet o Caja.", StopSign!, OK!)
		Else
			ll_nrocaja = il_NroCaja
			gl_packing = dw_1.Object.pafr_copack[ll_FilaPallet]
			
			dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			
			//	sle_1.Text							=	"ImprimiEndo Adhesivos en Formato GS1"
			ls_fecha									=	String(dw_1.Object.pafr_fecemb[1])
			ls_fecha									=	Left(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Right(ls_fecha, 2)
			dw_16.Object.Ole_1.Object.Text 	= 	"01" + ls_gtin_numero + "10" + ls_fecha /*+ "\F" + "21" + String(gl_packing, "0000") + &
														String(ll_inicio, '00000000')*/
			
			If ll_FilaPallet + 1 <= dw_1.RowCount() Then
				
				dw_16.Object.Ole_2.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
				
				ll_nrocaja = dw_1.Object.pafr_secuen[ll_FilaPallet + 1]
				gl_packing = dw_1.Object.pafr_copack[ll_FilaPallet + 1]
												
				dw_16.Object.Ole_2.Object.Text 	= 	"01" + ls_gtin_numero + "10" + ls_fecha /*+ "\F" + &
																"21" + String(gl_packing, "0000") + &
																String(ll_final, '00000000')*/
			End If
			//Voice Pick Code 
			iuo_voicecode	=	Create uo_voicecode
			iuo_voicecode.voicecode(dw_16	, dw_16.Object.emba_nroint[1],  ls_fecha, dw_16.Object.capr_fecemb[1], iuo_correl.foad_vopico)				
			
			dw_16.Print()
		End If		  
	NEXT
Else
	FOR ll_FilaPallet = 1 TO dw_1.RowCount()
							
		ll_NroPallet		=	dw_1.Object.paen_numero[ll_FilaPallet]
		il_NroPallet		=	ll_NroPallet
		il_NroCaja		=	dw_1.Object.pafr_secuen[ll_FilaPallet]
		li_packing		=  dw_1.Object.pafr_copack[ll_FilaPallet]
		dw_16.Reset()
		
		If integer(istr_mant.argumento[1]) <> gi_Cliebase Then
			ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),il_NroCaja,il_NroCaja,li_mercado,ls_camara,ii_sdp)
				dw_16.Object.calcod[1] =	ls_camara		
		Else
			ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),il_NroCaja,il_NroCaja,li_mercado)
			If ll_fila = 0 Then
				ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),li_packing,il_NroCaja,il_NroCaja,li_mercado)
			End If	
		End If	
	
		If ll_Fila = -1 Then
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
		ElseIf ll_Fila = 0 Then
			MessageBox( "Atención", "No Existe Información para este Pallet o Caja.", StopSign!, OK!)
		Else
			ll_nrocaja = il_NroCaja
			gl_packing = dw_1.Object.pafr_copack[ll_FilaPallet]
			
			dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			If dw_16.Object.Ole_1.Object.BarCode = 20 Then
	//			sle_1.Text								=	"ImprimiEndo Adhesivos en Formato CodeBar128"
				dw_16.Object.Ole_1.Object.Text 	= 	'00' + &
																String(dw_1.Object.zona_codigo[1],'00') + &
																String(dw_1.Object.plde_codigo[1],'0000') + &
																String(ll_nrocaja,'0000000000')
				
			ElseIf dw_16.Object.Ole_1.Object.BarCode = 88 Then
					ls_fecha								=	String(dw_1.Object.pafr_fecemb[1])
					ls_fecha								=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					
					If IsNull(dw_1.Object.prpr_prepro[1]) Or dw_1.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_1.Object.prpr_prepro[1]))) + dw_1.Object.prpr_prepro[1]
					End If
					
					ll_productor		=	long(dw_1.Object.prod_codigo[1])
					ls_codigo			=	"01" + ls_gtin_numero + "13" + ls_fecha /*+  String(ll_productor, '00000') + "\F"*/
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG // + "\F"
//					ls_codigo			=	ls_codigo	 +	"21" + String(dw_1.Object.plde_codigo[1], "0000") +	String(ll_nrocaja, '00000000') + "\F"
//					ls_codigo			=	ls_codigo	 +	"21" + String(dw_1.Object.pafr_copack[1], "0000") +	String(ll_nrocaja, '00000000') + "\F"
					
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_1.Object.emba_codigo[1]
					ls_Calibre	= dw_1.Object.pafr_calibr[1]
					
					ls_QR	=  String(dw_1.Object.plde_codigo[1], "0000") +	String(ll_nrocaja, '00000000') + Char(10) 
					ls_QR	+= 'Prod:' + String(ll_Productor, '00000') + Char(10) 
					ls_QR	+= 'Esp:' + String(dw_1.Object.espe_codigo[1], '00') + ' / ' + String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Pred:' + ls_CSG + ' / ' + String(dw_1.Object.pafr_cuart1[1],'000' )  + Char(10)
					ls_QR	+= 'Emb:' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= String(dw_1.Object.pafr_fecemb[1], 'dd/mm/yyyy')
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_16.Object.p_qrcode.FileName = ls_Ruta
					
					//Codigo UPC
					dw_16.Object.Ole_UPC.Object.Text 		= 	dw_1.Object.gtin_codupc[1]
					
					dw_16.Object.Ole_1.Object.Text 	= 	ls_codigo	
			End If
			/*
			Voice Pick Code 
			*/
			iuo_voicecode	=	Create uo_voicecode
			iuo_voicecode.voicecode(dw_16	, dw_16.Object.emba_nroint[1],  ls_fecha, dw_16.Object.capr_fecemb[1], iuo_correl.foad_vopico)		
			
			dw_16.Print()
		End If		  
	NEXT
End If	
end event

type pb_salir from w_para_informes`pb_salir within w_reimprecion_de_compactos
integer x = 2775
integer y = 924
integer taborder = 80
end type

type st_1 from statictext within w_reimprecion_de_compactos
integer x = 338
integer y = 596
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
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_reimprecion_de_compactos
integer x = 626
integer y = 580
integer width = 1253
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
	
	IF istr_mant.argumento[1]  = '590' THEN
		em_camara.Enabled = True
	ELSE	
		em_camara.Enabled = False
		em_camara.Text = ''
	END IF	

	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_reimprecion_de_compactos
integer x = 338
integer y = 728
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_reimprecion_de_compactos
integer x = 626
integer y = 716
integer width = 992
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	RETURN 1
END IF
end event

type em_numero from editmask within w_reimprecion_de_compactos
integer x = 626
integer y = 952
integer width = 489
integer height = 92
integer taborder = 40
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
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text


end event

type st_3 from statictext within w_reimprecion_de_compactos
integer x = 338
integer y = 960
integer width = 251
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
string text = "Pallet"
boolean focusrectangle = false
end type

type st_5 from statictext within w_reimprecion_de_compactos
integer x = 247
integer y = 440
integer width = 2354
integer height = 812
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_16 from datawindow within w_reimprecion_de_compactos
boolean visible = false
integer x = 3168
integer y = 340
integer width = 293
integer height = 208
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_carozos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_1 from radiobutton within w_reimprecion_de_compactos
integer x = 603
integer y = 472
integer width = 402
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
string text = "Pallet"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF This.Checked  THEN
	em_numero.Enabled = True
	em_1.Enabled = False
	em_1.Text = ''
END IF

em_numero.SetFocus()
end event

type rb_2 from radiobutton within w_reimprecion_de_compactos
integer x = 1746
integer y = 472
integer width = 402
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
string text = "Cajas"
boolean lefttext = true
end type

event clicked;IF This.Checked  THEN
	em_1.Enabled = True
	em_numero.Enabled = False
	em_numero.Text = ''
END IF

em_1.SetFocus()
end event

type em_1 from editmask within w_reimprecion_de_compactos
integer x = 1742
integer y = 940
integer width = 549
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text


end event

type st_4 from statictext within w_reimprecion_de_compactos
integer x = 1408
integer y = 960
integer width = 251
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
string text = "Nº Caja"
boolean focusrectangle = false
end type

type st_6 from statictext within w_reimprecion_de_compactos
integer x = 338
integer y = 1096
integer width = 251
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
string text = "Camara"
boolean focusrectangle = false
end type

type em_camara from editmask within w_reimprecion_de_compactos
integer x = 622
integer y = 1084
integer width = 718
integer height = 92
integer taborder = 70
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
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxxxxxxx"
double increment = 15
end type

event modified;istr_mant.argumento[4]	=	This.Text


end event

type st_7 from statictext within w_reimprecion_de_compactos
integer x = 338
integer y = 848
integer width = 283
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
string text = "Mercado"
boolean focusrectangle = false
end type

type dw_mercado from datawindow within w_reimprecion_de_compactos
integer x = 626
integer y = 832
integer width = 1006
integer height = 108
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
end type

event itemchanged;pb_acepta.Enabled = True
end event

type ddlb_sdp from dropdownlistbox within w_reimprecion_de_compactos
integer x = 1751
integer y = 1084
integer width = 480
integer height = 400
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string item[] = {"SDP","SAG"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	ii_sdp = 0
ELSE
	ii_sdp = 1
END IF	


end event

type dw_1 from datawindow within w_reimprecion_de_compactos
boolean visible = false
integer x = 3163
integer y = 88
integer width = 311
integer height = 236
integer taborder = 100
string title = "none"
string dataobject = "dw_mues_palletfruta_pallet_cajas"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

