$PBExportHeader$w_mant_mues_cajasprod_procalm.srw
forward
global type w_mant_mues_cajasprod_procalm from window
end type
type st_5 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_4 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_3 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_2 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_1 from statictext within w_mant_mues_cajasprod_procalm
end type
type vpb_control from vprogressbar within w_mant_mues_cajasprod_procalm
end type
type st_cal0 from statictext within w_mant_mues_cajasprod_procalm
end type
type sle_1 from statictext within w_mant_mues_cajasprod_procalm
end type
type pb_5 from picturebutton within w_mant_mues_cajasprod_procalm
end type
type sle_embaladora from singlelineedit within w_mant_mues_cajasprod_procalm
end type
type ole_1 from olecustomcontrol within w_mant_mues_cajasprod_procalm
end type
type r_cal1 from rectangle within w_mant_mues_cajasprod_procalm
end type
type r_cal2 from rectangle within w_mant_mues_cajasprod_procalm
end type
type r_cal3 from rectangle within w_mant_mues_cajasprod_procalm
end type
type sle_salida from statictext within w_mant_mues_cajasprod_procalm
end type
type st_odbc from statictext within w_mant_mues_cajasprod_procalm
end type
type st_cal1 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_a from statictext within w_mant_mues_cajasprod_procalm
end type
type st_b from statictext within w_mant_mues_cajasprod_procalm
end type
type st_cal2 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_cal3 from statictext within w_mant_mues_cajasprod_procalm
end type
type st_c from statictext within w_mant_mues_cajasprod_procalm
end type
type lb_1 from listbox within w_mant_mues_cajasprod_procalm
end type
type dw_4 from datawindow within w_mant_mues_cajasprod_procalm
end type
type dw_1 from datawindow within w_mant_mues_cajasprod_procalm
end type
type dw_2 from datawindow within w_mant_mues_cajasprod_procalm
end type
type p_rb from picture within w_mant_mues_cajasprod_procalm
end type
type dw_3 from datawindow within w_mant_mues_cajasprod_procalm
end type
end forward

global type w_mant_mues_cajasprod_procalm from window
integer width = 5710
integer height = 2540
boolean titlebar = true
string title = "CAJAS EN PRODUCCION"
boolean controlmenu = true
windowstate windowstate = maximized!
long backcolor = 16777215
string icon = "\Desarrollo 17\Imagenes\Sistemas\cajas_prod.ico"
boolean center = true
integer animationtime = 50
windowdockstate windowdockstate = windowdockstatetabbedwindow!
event ue_recuperadatos ( )
st_5 st_5
st_4 st_4
st_3 st_3
st_2 st_2
st_1 st_1
vpb_control vpb_control
st_cal0 st_cal0
sle_1 sle_1
pb_5 pb_5
sle_embaladora sle_embaladora
ole_1 ole_1
r_cal1 r_cal1
r_cal2 r_cal2
r_cal3 r_cal3
sle_salida sle_salida
st_odbc st_odbc
st_cal1 st_cal1
st_a st_a
st_b st_b
st_cal2 st_cal2
st_cal3 st_cal3
st_c st_c
lb_1 lb_1
dw_4 dw_4
dw_1 dw_1
dw_2 dw_2
p_rb p_rb
dw_3 dw_3
end type
global w_mant_mues_cajasprod_procalm w_mant_mues_cajasprod_procalm

type variables
String						is_computador, is_titulo, is_secuencia
Integer					ii_OF
Long						il_primero
Boolean					ib_Procesa = True, ib_impresora, ib_ImpresoraPrimerCambio
uo_manejoimpresora	iuo_impresora
uo_voicecode			iuo_voicecode
uo_QR					iuo_QR
end variables

forward prototypes
public function boolean generacaja (string as_lectura)
public subroutine calibres ()
public subroutine wf_bloqueacolumnas ()
public subroutine settransobject ()
end prototypes

event ue_recuperadatos();Integer	li_cliente, respuesta, li_filas, li_fila, li_Salida = 0
Long		ll_planta, ll_nrocaja, ll_packing, ll_productor
String		ls_fecha, ls_codigo, ls_tipo, ls_caja, ls_CSG, ls_QR, ls_Ruta, ls_embalaje, ls_Calibre

li_cliente		=	dw_2.Object.clie_codigo[1]
ll_planta		=	dw_2.Object.plde_codigo[1]
ll_nrocaja	=	dw_2.Object.nrocaja[1]
il_primero	=	ll_nrocaja

DO
	vpb_control.Position = 0
	li_filas	=	dw_1.Retrieve(li_cliente, ll_planta, il_primero, ll_nrocaja, 1)
	
	If li_filas = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)										
	ElseIf li_filas > 0 Then
		DO
			vpb_control.StepIt()
			li_filas	=	dw_3.Retrieve(li_cliente, ll_planta, ll_nrocaja)
			
			If li_filas = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			
			ElseIf li_filas > 0 Then
				dw_1.Object.Ole_1.Object.BarCode	=	Integer(dw_2.Object.prpa_codbar[1])
				
				vpb_control.StepIt()
				If dw_1.Object.Ole_1.Object.BarCode = 20 Then
					sle_salida.Text						=	is_titulo + "     /     Formato CodeBar128"
					dw_1.Object.Ole_1.Object.Text 	= 	'00' + &
																	String(dw_1.Object.zona_codigo[1],'00') + &
																	String(dw_1.Object.plde_codigo[1],'0000') + &
																	String(dw_1.Object.capr_numero[1],'0000000000')
				
				ElseIf dw_1.Object.Ole_1.Object.BarCode = 88 Then
					sle_salida.Text						=	is_titulo + "     /     Formato GS1"
					ls_fecha								=	String(dw_1.Object.capr_fecemb[1])
					ls_fecha								=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					
					If IsNull(dw_1.Object.emba_nroint[1]) OR Len(String(dw_1.Object.emba_nroint[1])) <> 14 Then
						MessageBox("Error de Codigo", "Existe un problema con el codigo GTIN14 para el embalaje utilizado," + &
																"~r~nFavor de comunicar situación a encargado de la linea")
						Return
					End If
					
					If IsNull(dw_1.Object.prpr_prepro[1]) Or dw_1.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_1.Object.prpr_prepro[1]))) + dw_1.Object.prpr_prepro[1]
					End If
					
					ll_productor		=	long(dw_1.Object.prod_codigo[1])
					ls_codigo			=	"01" + dw_1.Object.emba_nroint[1] + "13" + ls_fecha /*+  String(ll_productor, '00000')*/ + "\F"
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  + "\F"
					ls_codigo			=	ls_codigo	 +	"21" + String(dw_1.Object.plde_codigo[1], "0000") +	String(ll_nrocaja, '00000000') + "\F"
					
					dw_1.Object.Ole_1.Object.Text 	= 	ls_codigo
					
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_1.Object.emba_codigo[1]
					ls_Calibre	= dw_1.Object.capr_calibr[1]
					
//					ls_QR = String(ll_nrocaja, '00000000') //+ Char(10) 
					ls_QR	= 'Productor:	' + String(ll_Productor, '00000') + Char(10) 
					ls_QR	+= 'Especie: 	' + String(dw_1.Object.espe_codigo[1], '00') + ' / ' + String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Predio: ' + ls_CSG + ' / ' + String(dw_1.Object.prod_cuarte[1],'000' )  + Char(10)
					ls_QR	+= 'Emb.: 	' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= 'Fecha Emb.: ' + String(dw_1.Object.capr_fecemb[1], 'dd/mm/yyyy')
				
//					ls_QR	+= String(ll_Productor, '00000') + '/' + ls_CSG + ' / ' + String(dw_1.Object.prod_cuarte[1],'000' )  + Char(10)
//					ls_QR	+= String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
//					ls_QR	+= ls_Embalaje + ' / ' + ls_Calibre + Char(10)
//					ls_QR	+= String(dw_1.Object.capr_fecemb[1], 'dd/mm/yyyy')
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_1.Object.p_qrcode.FileName = ls_Ruta

					/*
					Code Pick Voice
					*/
					iuo_voicecode	=	Create uo_voicecode
					iuo_voicecode.voicecode(dw_1	, dw_1.Object.emba_nroint[1],  ls_fecha, dw_1.Object.capr_fecemb[1], dw_2.Object.codvoz[1])
					
					If Not IsNull(dw_2.Object.salseg[1]) Then li_Salida = dw_2.Object.salseg[1]
					
					If dw_2.Object.ocx[1] = 2 Then
						is_secuencia							=	String(li_Salida, '00') + String(dw_1.Object.capr_numero[1]) 
						dw_1.Object.Ole_2.Object.BarCode = 	20 
						dw_1.Object.Ole_2.Object.Text		=	is_secuencia
					End If
				End If
				
				vpb_control.StepIt()
				
				If IsNull(dw_2.Object.impresora[1]) OR Len(String(dw_2.Object.impresora[1])) < 1 Then
					ib_impresora	=	False
				Else
					iuo_impresora.asignaimpresora_comp(dw_2.Object.impresora[1])
					ib_impresora	=	True
				End If
			
				If iuo_impresora.is_impresoracomp <> '' AND ib_impresora Then
					iuo_impresora.setimprcomp()
					
					dw_1.AcceptText()
					dw_1.Print(False, False)
					
					iuo_impresora.setimprdef()
				Else
					dw_1.AcceptText()
					dw_1.Print(False, False)
				End If
			Else
				MessageBox("Problema de Datos",  "El registro de la caja ha sido creado, pero no es posible generar el compacto."+&
														 	"~r~nFavor comunicar situación al Encargado del Sistema.", Exclamation!)
			End If
		LOOP WHILE respuesta = 1
		
	Else
		MessageBox("Problema de Datos",  "El registro de la caja ha sido creado, pero no es posible generar el compacto."+&
													"~r~nFavor comunicar situación al Encargado del Sistema.", Exclamation!)
	End If
LOOP WHILE respuesta = 1

commit;
vpb_control.StepIt()
end event

public function boolean generacaja (string as_lectura);Integer	li_salida, li_fila, li_Posicion
String		ls_lado, ls_mensaje, ls_secuen, ls_Orden, ls_Calibre, ls_Color,ls_embalaje, ls_String
Long		ll_embala, ll_secuen, ll_planta


If dw_4.Object.lisa_lectur[1] > 1 Then
	is_computador 	= 	'SALIDA' + Mid(as_lectura, Pos(as_lectura,'S',1)+1, 2)
	li_salida 			= 	-1
	ls_lado			=	 Mid(as_lectura, Pos(as_lectura,'S',1)+1, 3)//'*'//'00' +
	ll_embala		=	Long(Mid(as_lectura,Pos(as_lectura,'CE',1) + 2, 5))
	ls_secuen		=	Mid(as_lectura,Pos(as_lectura,'CE',1) + 7, 5)//Mid(as_lectura, 9, 5)
	If ls_secuen <> "" Then
		ll_secuen	=	Long(ls_secuen)
	Else
		ll_secuen	=	0		
	End If
ElseIf Mid(as_lectura, 2, 2) = 'CE' Then
	If Pos(as_lectura, '.NS', 1) > 0 Then
		li_salida	= 	Integer(Mid(as_lectura, Pos(as_lectura, '.NS', 1) + 3, 2))
	Else
		li_salida 	= 	-1		
	End If
	
	If UPPER(Left(as_lectura, 1)) <> 'A' AND &
		UPPER(Left(as_lectura, 1)) <> 'B' AND &
		UPPER(Left(as_lectura, 1)) <> 'C' Then
		ls_lado		=	'*'
	Else
		If gi_manejo = 0 Then
			If Pos(as_lectura, '.NS', 1) > 0 Then
				ls_lado		=	String(li_salida, '00') + Left(as_lectura, 1)
			Else
				ls_lado		=	String(dw_4.Object.lisa_codigo[1], '00') + Left(as_lectura, 1)
			End If			
		Else
			ls_lado		=	String('00') + Left(as_lectura, 1)			
		End If		
	End If
	
	ll_embala	=	Long(Mid(as_lectura, 4, 5))
	ls_secuen	=	Mid(as_lectura, 9, 5)
	
	If ls_secuen <> "" Then
		ll_secuen	=	Long(ls_secuen)
	Else
		ll_secuen	=	0		
	End If
	
	If ll_secuen = 0 Then
		MessageBox("Error", "La secuencia no es valida", StopSign!)
		Return False
	End If
	
ElseIf IsNumber(Mid(as_lectura, 1, 2)) Then	
	li_salida 		= 	Integer(Left(as_lectura, 2))
	ls_lado			=	Left(as_lectura, 3)
	ll_embala		=	-1
	ll_secuen		=	Long(Right(as_lectura, len(as_lectura) - 3))
ElseIf Mid(as_lectura, 1, 3) = "UDP" Then
	is_computador 	= 	'UDP' + Mid(as_lectura, 4, 2)
	li_salida 			= 	Integer(Mid(as_lectura, 4, 2))
	ls_lado			=	Mid(as_lectura, 4, 3)
	ll_embala		=	-1
	ll_secuen			=	-1
ElseIf Mid(as_lectura, 1, 2) = "OF" Then
	is_computador 	= 	is_Computador
	ls_String			=	Mid(as_lectura,3)
	ls_Orden 		=	Mid(ls_String, 1, Pos(Mid(ls_String,li_Posicion),'&') -1)
	li_Posicion  		=	Pos(ls_String,'&') 	+ 1	
	ls_String			=	Mid(ls_String, li_Posicion)	
	li_salida	 		=	Integer(Mid(ls_String,  1 , Pos(ls_String,'&') -1))
	li_Posicion  		= 	Pos(ls_String,'&') + 1
	ls_String			=	Mid(ls_String, li_Posicion)		
	ls_Calibre 		= 	Mid(ls_String,  1, Pos(ls_String,'&') -1)
	li_Posicion		=	Pos(ls_String,'&') + 1
	ls_String			=	Mid(ls_String, li_Posicion)
	ls_Color 			=	Mid(ls_String,  1, Pos(ls_String,'&') -1)
	li_Posicion  		=	Pos(ls_String,'&') + 1
	ls_String			=	Mid(ls_String, li_Posicion)
	ls_embalaje 	= 	Mid(ls_String,  1, Pos(ls_String,'&') - 1)
	ll_embala		=	-1
	ll_secuen			=	-1
Else
	//MessageBox("Atención", "El código de barras leido no es valido.", StopSign!) Comentado por A.O.
	Return False
End If

is_secuencia		=	ls_lado	

sqlca.Autocommit	=	False
ll_planta			=	gstr_ParamPlanta.CodigoPlanta
li_fila 				= 	dw_2.Retrieve(ll_planta, is_computador, li_salida, ls_lado, 	 ll_embala, 	 ll_secuen, as_lectura)

If li_fila = 1 OR (gi_manejo = 1 AND li_fila > 0) Then
	If dw_2.Object.clie_codigo[1] = -1 Then
		ls_mensaje	=	"Error (" + String(dw_2.Object.nroerror[1]) + ") : " + dw_2.Object.formato[1]//  comentado  A.O.
		MessageBox("Error", ls_mensaje, StopSign!, Ok!, 1)
		Rollback;
		Return False	
	Else
		Commit;
		dw_1.DataObject	=	dw_2.Object.formato[1]
		dw_1.SetTransObject(sqlca)
		Return True
	End If
Else
	Rollback;
	Return False
End If
end function

public subroutine calibres ();environment lenv_display
Long 			ll_screenWidth,ll_screenHeight
Boolean		lb_embalaje, lb_calibre
Integer		li_filas, li_actual, li_codigos
String			ls_embalaje, ls_calibre

IF dw_4.Retrieve(is_Computador) > 0 THEN
	sle_salida.Text 			=	"Linea " 	+ 	String(dw_4.Object.line_codigo[1]) + '     /     ' + &
									"Salida " 	+ 	String(dw_4.Object.lisa_codigo[1]) + '     /     ' + &
									"PC " 		+ 	Upper(dw_4.Object.equi_nombre[1])
	li_codigos	=	dw_4.Object.loco_modcod[1]
	IF  li_codigos > 1 THEN
		sle_salida.Text 		=	sle_salida.Text + '     /     ' + String(dw_4.Object.loco_modcod[1]) + " Codigos"
	END IF
	
	ii_OF = 	dw_4.Object.line_overflo[1]
	
	is_titulo					=	sle_salida.Text
	
	ls_embalaje	=	dw_4.Object.emba_codigo[1]
	ls_calibre	=	dw_4.Object.prsd_calibr[1]
	lb_embalaje	=	False
	lb_calibre	=	False
	
	IF dw_4.RowCount() > 1 THEN
		FOR li_filas = 2 TO dw_4.RowCount()
			IF ls_embalaje <> dw_4.Object.emba_codigo[li_filas] THEN lb_embalaje	=	True
			IF ls_calibre  <> dw_4.Object.prsd_calibr[li_filas] THEN lb_calibre	=	True
		NEXT
	ELSE
		lb_embalaje	=	True
		lb_calibre	=	True
	END IF
	
	IF lb_embalaje AND lb_calibre THEN
		st_cal0.Text	=	'Embalaje / Calibre'
		
	ELSEIF lb_embalaje THEN
		st_cal0.Text	=	'Embalajes'
		
	ELSEIF lb_calibre THEN
		st_cal0.Text	=	'Calibres'
		
	END IF
ELSE
	MessageBox("Error", "Este PC no se encuentra asociado a una salida programada en la orden activa, ~r~r" + &
							  "o bien no existe una orden activa. Favor comunicarse con el encargado", Exclamation!)
	Return
END IF

IF GetEnvironment(lenv_display) = 1 THEN
	ll_screenWidth 	= 	PixelsToUnits(lenv_display.screenwidth,	XPixelsToUnits!)
	ll_screenHeight 	= 	PixelsToUnits(lenv_display.screenheight,	YPixelsToUnits!)

	st_cal0.y			=	10
	st_cal0.x			=	(ll_screenWidth - st_cal0.width) 			/ 2
	
	r_cal1.FillColor	=	RGB(37, 	255, 	6)
	r_cal2.FillColor	=	RGB(242,	0, 	6)
	r_cal3.FillColor	=	RGB(0, 	43, 	242)

	st_cal1.BackColor	=	RGB(37, 	255, 	6)
	st_a.BackColor		=	RGB(37, 	255, 	6)
	st_cal2.BackColor	=	RGB(242,	0, 	6)
	st_b.BackColor		=	RGB(242,	0, 	6)
	st_cal3.BackColor	=	RGB(0, 	43, 	242)
	st_c.BackColor		=	RGB(0, 	43, 	242)

	r_cal1.Visible		=	False
	r_cal2.Visible		=	False
	r_cal3.Visible		=	False

	st_cal1.Visible	=	False
	st_cal2.Visible	=	False
	st_cal3.Visible	=	False

	st_cal1.Text		=	''
	st_cal2.Text		=	''
	st_cal3.Text		=	''

	CHOOSE CASE dw_4.RowCount()
		CASE 1
			r_cal1.Visible		=	True
			r_cal1.Width		=	ll_screenWidth
			r_cal1.Height		=	ll_screenHeight
			r_cal1.y				=	0
			r_cal1.x				=	0

			IF lb_embalaje THEN
				st_cal1.Text	=	dw_4.Object.emba_codigo[1] + ' '
			END IF

			IF lb_calibre THEN
				st_cal1.Text	=	st_cal1.Text + dw_4.Object.prsd_calibr[1]
			END IF

			st_cal1.x			=	(r_cal1.Width / 2) + r_cal1.y - (st_cal1.Width / 2) 
			st_cal1.y			=	st_cal0.y + st_cal0.Height + 10
			st_cal1.Visible	=	True

		CASE 2
			r_cal1.Visible		=	True
			r_cal1.Width		=	ll_screenWidth / 2
			r_cal1.Height		=	ll_screenHeight
			r_cal1.y				=	0
			r_cal1.x				=	0

			IF lb_embalaje THEN
				st_cal1.Text	=	dw_4.Object.emba_codigo[1] + ' '
			END IF

			IF lb_calibre THEN
				st_cal1.Text	=	st_cal1.Text + dw_4.Object.prsd_calibr[1]
			END IF

			st_cal1.x			=	(r_cal1.Width / 2) + r_cal1.y - (st_cal1.Width / 2)
			st_cal1.y			=	st_cal0.y + st_cal0.Height + 10
			
			r_cal2.Visible		=	True
			r_cal2.Width		=	ll_screenWidth / 2
			r_cal2.Height		=	ll_screenHeight
			r_cal2.y				=	0

			r_cal2.x				=	r_cal1.Width + 1
			IF lb_embalaje THEN
				st_cal2.Text	=	dw_4.Object.emba_codigo[2] + ' '
			END IF

			IF lb_calibre THEN
				st_cal2.Text	=	st_cal2.Text + dw_4.Object.prsd_calibr[2]
			END IF

			st_cal2.x			=	r_cal1.Width + 1 + (r_cal2.Width / 2) + r_cal2.y - (st_cal2.Width / 2)
			st_cal2.y			=	st_cal0.y + st_cal0.Height + 10

			st_cal1.Visible	=	True
			st_cal2.Visible	=	True

		CASE 3
			r_cal1.Visible		=	True
			r_cal1.Width		=	ll_screenWidth / 3
			r_cal1.Height		=	ll_screenHeight
			r_cal1.y				=	0
			r_cal1.x				=	0

			IF lb_embalaje THEN
				st_cal1.Text	=	dw_4.Object.emba_codigo[1] + ' '
			END IF

			IF lb_calibre THEN
				st_cal1.Text	=	st_cal1.Text + dw_4.Object.prsd_calibr[1]
			END IF

			st_cal1.x			=	(r_cal1.Width / 2) + r_cal1.y - (st_cal1.Width / 2)
			st_cal1.y			=	st_cal0.y + st_cal0.Height + 10

			r_cal2.Visible		=	TRUE
			r_cal2.Width		=	ll_screenWidth / 3
			r_cal2.Height		=	ll_screenHeight
			r_cal2.y				=	0
			r_cal2.x				=	r_cal1.Width + 1

			IF lb_embalaje THEN
				st_cal2.Text	=	dw_4.Object.emba_codigo[2] + ' '
			END IF

			IF lb_calibre THEN
				st_cal2.Text	=	st_cal2.Text + dw_4.Object.prsd_calibr[2]
			END IF

			st_cal2.x			=	r_cal1.Width + 1 + (r_cal2.Width / 2) + r_cal2.y - (st_cal2.Width / 2)
			st_cal2.y			=	st_cal0.y + st_cal0.Height + 10

			r_cal3.Visible		=	TRUE
			r_cal3.Width		=	ll_screenWidth / 3
			r_cal3.Height		=	ll_screenHeight
			r_cal3.y				=	0
			r_cal3.x				=	r_cal1.Width + 1 + r_cal2.Width + 1

			IF lb_embalaje THEN
				st_cal3.Text	=	dw_4.Object.emba_codigo[3] + ' '
			END IF

			IF lb_calibre THEN
				st_cal3.Text	=	st_cal3.Text + dw_4.Object.prsd_calibr[3]
			END IF

			st_cal3.x			=	r_cal1.Width + 1 + r_cal2.Width + 1 + (r_cal3.Width / 2) + r_cal3.y - (st_cal3.Width / 2)
			st_cal3.y			=	st_cal0.y + st_cal0.Height + 10

			st_cal1.Visible	=	True
			st_cal2.Visible	=	True
			st_cal3.Visible	=	True

	END CHOOSE
	
END IF

st_a.x				=	st_cal1.x
st_a.y				=	sle_embaladora.y + sle_embaladora.Height + 10
st_a.Visible		=	st_cal1.Visible

st_b.x				=	st_cal2.x
st_b.y				=	sle_embaladora.y + sle_embaladora.Height + 10
st_b.Visible		=	st_cal2.Visible

st_c.x				=	st_cal3.x
st_c.y				=	sle_embaladora.y + sle_embaladora.Height + 10
st_c.Visible		=	st_cal3.Visible
end subroutine

public subroutine wf_bloqueacolumnas ();
end subroutine

public subroutine settransobject ();dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
end subroutine

event resize;Long 			ll_screenWidth,ll_screenHeight
long 			ll_start, ll_used, ll_height
environment lenv_display

This.WindowState	=	Maximized!

IF GetEnvironment(lenv_display) = 1 THEN
	ll_screenWidth 			= 	PixelsToUnits(lenv_display.screenwidth,	XPixelsToUnits!)
	ll_screenHeight 		= 	PixelsToUnits(lenv_display.screenheight,	YPixelsToUnits!)
	
	ll_height					=	( ll_screenHeight - ( dw_3.height + sle_embaladora.height ) ) / 2
	st_cal0.x					=	(ll_screenWidth - st_cal0.width) 		/ 2
	st_cal0.y					=   ll_screenHeight - (ll_screenHeight - 300)//st_cal0.y - st_cal0.Height)
	dw_3.x 					= 	(ll_screenWidth - dw_3.width) 			/ 2
	dw_3.y 				=	st_cal0.y + st_cal0.Height + 120
	
	sle_embaladora.x 		= 	dw_3.x//(ll_screenWidth - sle_embaladora.width) 			// 2
	sle_embaladora.y	 	=	dw_3.y + dw_3.Height + 1
	

	pb_5.Visible				=	True
	pb_5.x					=	This.Width - pb_5.width - 160
	pb_5.y					=	sle_embaladora.y + sle_embaladora.Height + 2
	
	sle_1.x 					= dw_3.x//(ll_screenWidth - sle_embaladora.width) 			// 2
	
	sle_salida.x				=	dw_3.x//(ll_screenWidth - sle_salida.width) 			// 2
	sle_salida.y				=	This.Height - sle_salida.Height - st_odbc.Height - 170
	
	st_odbc.x					=	dw_3.x//(ll_screenWidth - st_odbc.width) 			// 2
	st_odbc.y					=	sle_salida.y + sle_salida.Height + 10

END IF
end event

on w_mant_mues_cajasprod_procalm.create
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.vpb_control=create vpb_control
this.st_cal0=create st_cal0
this.sle_1=create sle_1
this.pb_5=create pb_5
this.sle_embaladora=create sle_embaladora
this.ole_1=create ole_1
this.r_cal1=create r_cal1
this.r_cal2=create r_cal2
this.r_cal3=create r_cal3
this.sle_salida=create sle_salida
this.st_odbc=create st_odbc
this.st_cal1=create st_cal1
this.st_a=create st_a
this.st_b=create st_b
this.st_cal2=create st_cal2
this.st_cal3=create st_cal3
this.st_c=create st_c
this.lb_1=create lb_1
this.dw_4=create dw_4
this.dw_1=create dw_1
this.dw_2=create dw_2
this.p_rb=create p_rb
this.dw_3=create dw_3
this.Control[]={this.st_5,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_1,&
this.vpb_control,&
this.st_cal0,&
this.sle_1,&
this.pb_5,&
this.sle_embaladora,&
this.ole_1,&
this.r_cal1,&
this.r_cal2,&
this.r_cal3,&
this.sle_salida,&
this.st_odbc,&
this.st_cal1,&
this.st_a,&
this.st_b,&
this.st_cal2,&
this.st_cal3,&
this.st_c,&
this.lb_1,&
this.dw_4,&
this.dw_1,&
this.dw_2,&
this.p_rb,&
this.dw_3}
end on

on w_mant_mues_cajasprod_procalm.destroy
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.vpb_control)
destroy(this.st_cal0)
destroy(this.sle_1)
destroy(this.pb_5)
destroy(this.sle_embaladora)
destroy(this.ole_1)
destroy(this.r_cal1)
destroy(this.r_cal2)
destroy(this.r_cal3)
destroy(this.sle_salida)
destroy(this.st_odbc)
destroy(this.st_cal1)
destroy(this.st_a)
destroy(this.st_b)
destroy(this.st_cal2)
destroy(this.st_cal3)
destroy(this.st_c)
destroy(this.lb_1)
destroy(this.dw_4)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.p_rb)
destroy(this.dw_3)
end on

event open;RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, is_Computador)

iuo_impresora	=	Create uo_manejoimpresora
iuo_QR			=	Create uo_QR

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)

dw_3.InsertRow(0)
If dw_4.Retrieve(is_Computador) = 0 Then
	 MessageBox(	"Atención", "Equipo no esta asignado a Salida de la Linea o no esta dentro de la Programación Realizada~~" + &
										"Por Favor Revisa Configuraciones",Information!)
	Close(This)	
Else
	ii_OF = 	dw_4.Object.line_oveflo[1]
	ole_1.object.LicenseMe("Mem: Exportadora Rio Blanco Santiago CL", 3, 1, &
							  "33E3AD94226C68E043BEF94763700EA9", 44)

	IF gstr_paramplanta.prpa_ctludp = 1 THEN 
		Timer(5)	
	ELSE
		Timer(0)	
	END IF
	
	ib_ImpresoraPrimerCambio	=	False
	IF NOT Conexion() THEN Close(This)
End If
end event

event timer;Int		li_Fila
Dec	ld_Tara, ld_Pesobruto
String	ls_Salida

Timer(0)

Yield()
lb_1.Reset()

IF ii_OF = 1 THEN
	/*Carga Datos a ListBox 
		de la ruta indicada en la linea, donde está  asignado el Equipo
	*/
	lb_1.DirList(dw_4.Object.line_direct[1] + '\*.OF*', 33)
	
	/*
		Borrar Archivos 
	*/
	FOR li_Fila = 1 To lb_1.TotalItems()
		FileDelete(lb_1.Text(li_Fila))	
	NEXT
	
	FOR li_Fila = 1 To lb_1.TotalItems()
		sle_embaladora.Text = ''
		ls_Salida = 'OF' +Mid(lb_1.Text(li_Fila), 1, Pos(lb_1.Text(li_Fila),'.') - 1)
		sle_embaladora.Text =ls_Salida
		sle_embaladora.TriggerEvent('Modified')
	NEXT
ELSE	
	/*Carga Datos a ListBox 		de la ruta indicada en la linea, donde está  asignado el Equipo	*/
	lb_1.DirList(GetCurrentDirectory() + '\*.udp*', 33)
	/*		Borrar Archivos 	*/
	FOR li_Fila = 1 To lb_1.TotalItems()
		FileDelete(lb_1.Text(li_Fila))	
	NEXT
	
	FOR li_Fila = 1 To lb_1.TotalItems()
		sle_embaladora.Text = ''
		ls_Salida = Mid(lb_1.Text(li_Fila), 1, Pos(lb_1.Text(li_Fila),'.') - 1)
		ls_Salida = 'UDP' + Fill('0', 2 - Len(ls_Salida)) + ls_Salida //+ 'A'
		sle_embaladora.Text = ls_Salida
		sle_embaladora.TriggerEvent('Modified')		
	NEXT
END IF

lb_1.Reset()
GarbageCollect()

Timer(1)
end event

event closequery;iuo_impresora.setimprdef()
end event

type st_5 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 123
integer y = 1884
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Retrieve 1"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 119
integer y = 892
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Listo"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 123
integer y = 1156
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Impresion"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 123
integer y = 1384
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Asignación"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 133
integer y = 1640
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Retrieve 2"
boolean focusrectangle = false
end type

type vpb_control from vprogressbar within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 9
integer y = 884
integer width = 110
integer height = 1068
unsignedinteger maxposition = 100
integer setstep = 25
boolean smoothscroll = true
end type

type st_cal0 from statictext within w_mant_mues_cajasprod_procalm
integer x = 649
integer y = 12
integer width = 4192
integer height = 316
integer textsize = -48
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "SERVIDOR DE IMPRESION"
alignment alignment = center!
boolean border = true
long bordercolor = 21251140
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type sle_1 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 841
integer y = 1532
integer width = 3520
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_5 from picturebutton within w_mant_mues_cajasprod_procalm
integer x = 5193
integer y = 1652
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type sle_embaladora from singlelineedit within w_mant_mues_cajasprod_procalm
integer x = 649
integer y = 1464
integer width = 4192
integer height = 396
integer taborder = 10
boolean bringtotop = true
integer textsize = -48
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;Integer	li_findA, li_findB, li_Marca, li_Marca2
dw_3.Reset()
dw_3.InsertRow(0)

IF dw_4.Object.lisa_lectur[1] > 1 THEN
	sle_embaladora.Text = '*' + sle_embaladora.Text
	
	if Pos(sle_embaladora.Text, '*', 1) > 0 THEN li_marca = 1
	IF Pos(sle_embaladora.Text, '*',  Pos(sle_embaladora.Text, '*', 1) + 1)> 0 THEN li_marca2 = 1
	IF (li_Marca + li_Marca2) <= 1 THEN
		This.SetFocus()
		Return
	END IF
END IF

IF dw_4.Object.loco_modcod[1] > 1 THEN
	CHOOSE CASE dw_4.Object.loco_modcod[1]
		CASE 2
			li_findA = Pos(This.Text, 'CE', 1)
			li_findB = Pos(This.Text, '.NS', 1)
			
			IF li_findA * li_findB = 0 THEN 
				This.SetFocus()
				Return
			END IF
	END CHOOSE
END IF

IF gi_manejo = 0 THEN
	dw_2.DataObject	=	"dw_creacion_cajas"
	dw_3.DataObject	=	"dw_mant_mues_spro_cajasprod_clon"
	dw_3.SetTransObject(sqlca)
	dw_2.SetTransObject(sqlca)
	dw_3.InsertRow(0)
ELSE
	dw_2.DataObject	=	"dw_creacion_control_cajas"
	dw_3.DataObject	=	"dw_mant_mues_spro_cajaembadetalle"
	dw_3.SetTransObject(sqlca)
	dw_2.SetTransObject(sqlca)
	dw_3.InsertRow(0)
END IF
//25
IF GeneraCaja(This.Text) THEN
	IF gi_manejo = 0 THEN
		//50
		//Calibres()
		//75
		Parent.TriggerEvent("ue_recuperadatos")
		//100
	ELSE
		dw_3.Object.caem_correl.Background.color	=	rgb(0, 0, 255)
		dw_3.Retrieve(gstr_ParamPlanta.CodigoPlanta, dw_2.Object.clie_codigo[1], dw_2.Object.nrocaja[1])
	END IF
ELSE
	IF gi_manejo <> 0 THEN
		dw_3.Object.caem_correl.Background.color	=	rgb(255, 0, 0)
	END IF
END IF

This.Text	=	''
This.SetFocus()
end event

type ole_1 from olecustomcontrol within w_mant_mues_cajasprod_procalm
event click ( )
event dblclick ( )
event mousedown ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mousemove ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mouseup ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event beforedraw ( )
boolean visible = false
integer x = 1883
integer y = 1776
integer width = 1211
integer height = 264
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_mant_mues_cajasprod_procalm.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type r_cal1 from rectangle within w_mant_mues_cajasprod_procalm
boolean visible = false
long linecolor = 33554432
linestyle linestyle = transparent!
integer linethickness = 4
long fillcolor = 1073741824
integer x = 1376
integer y = 1728
integer width = 357
integer height = 184
end type

type r_cal2 from rectangle within w_mant_mues_cajasprod_procalm
boolean visible = false
long linecolor = 33554432
linestyle linestyle = transparent!
integer linethickness = 4
long fillcolor = 33543637
integer x = 4590
integer y = 380
integer width = 357
integer height = 184
end type

type r_cal3 from rectangle within w_mant_mues_cajasprod_procalm
boolean visible = false
long linecolor = 33554432
linestyle linestyle = transparent!
integer linethickness = 4
long fillcolor = 1073741824
integer x = 4187
integer y = 380
integer width = 357
integer height = 184
end type

type sle_salida from statictext within w_mant_mues_cajasprod_procalm
integer x = 649
integer y = 2044
integer width = 4192
integer height = 120
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_odbc from statictext within w_mant_mues_cajasprod_procalm
integer x = 649
integer y = 1876
integer width = 4192
integer height = 120
boolean bringtotop = true
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 16711680
string text = "Sin Conexion ODBC"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_cal1 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 617
integer y = 396
integer width = 1696
integer height = 336
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 33543637
string text = "XL1"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_a from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 617
integer y = 740
integer width = 1696
integer height = 336
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 33543637
string text = "A"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_b from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 2318
integer y = 740
integer width = 1696
integer height = 336
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 33543637
string text = "B"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_cal2 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 2318
integer y = 396
integer width = 1696
integer height = 336
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 33543637
string text = "XL2"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_cal3 from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 4018
integer y = 396
integer width = 1696
integer height = 336
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 33543637
string text = "XL3"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_c from statictext within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 4018
integer y = 740
integer width = 1696
integer height = 336
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 33543637
string text = "C"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type lb_1 from listbox within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 334
integer y = 424
integer width = 201
integer height = 192
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 64
integer y = 432
integer width = 242
integer height = 184
boolean bringtotop = true
string title = "4"
string dataobject = "dw_mues_calibres_salida"
end type

type dw_1 from datawindow within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 64
integer y = 632
integer width = 343
integer height = 224
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_gtin14"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_mant_mues_cajasprod_procalm
boolean visible = false
integer x = 192
integer y = 1740
integer width = 4818
integer height = 380
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_creacion_cajas"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type p_rb from picture within w_mant_mues_cajasprod_procalm
integer x = 37
integer y = 20
integer width = 521
integer height = 396
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_mues_cajasprod_procalm
event key pbm_dwnkey
integer x = 649
integer y = 28
integer width = 4192
integer height = 1428
string title = "none"
string dataobject = "dw_mant_mues_spro_cajasprod_clon"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
09w_mant_mues_cajasprod_procalm.bin 
2D00001400e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000006fffffffe000000040000000500000007fffffffe00000008fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000003000000000000000000000000000000000000000000000000000000003178477001d8810700000003000008c00000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000002e0000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000010000043500000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a00000002000000010000000485382357404fca79c1bc00b25884d197000000003178206001d881073178477001d88107000000000000000000000000fffffffe00000002000000030000000400000005000000060000000700000008000000090000000a0000000b0000000c0000000d0000000e0000000f0000001000000011fffffffe000000130000001400000015000000160000001700000018000000190000001a0000001b0000001c0000001d0000001e0000001f000000200000002100000022fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
28ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0079004d00720020006e00750074002d006d0069002000650069006c006500630073006e002000650065006b00000079000000000000000000000000000000000000090000001b63000006d2ffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c00001367080000000000520031003000370031003000380031003000380031003200350030003000370031003000310039003000340030003400320046005c00310032003000320035003000300030003000300030003000310030000300000000000000580003000300000000000100000003000b00000008ffff0000000000ff00030003ffffff000000000800080033000000390033000300000000000000ff000b0000000bff0000080008000000000030004200310032003a003a0042004200330034003a003a0042005300310032003a003a0053005300330034003a0000005300200008003100000032003a0033003a0034003a0031003a0032003a0033003a0034003a001300000000000000520008004c0000002000610070006f007200650063006100f300690020006e0065007300630020006d006f006c007000740065002000f3006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000070a00000000003400b0000000500000000000000f00000000000033f0b000000080000000000000000000500000000000300000000000000560003000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000009ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000090000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000090000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000900000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000090000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000900000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000903000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000090000001b63000006d2ffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c00001367080000000000520031003000370031003000380031003000380031003200350030003000370031003000310039003000340030003400320046005c00310032003000320035003000300030003000300030003000310030000300000000000000580003000300000000000100000003000b00000008ffff0000000000ff00030003ffffff000000000800080033000000390033000300000000000000ff000b0000000bff0000080008000000000030004200310032003a003a0042004200330034003a003a0042005300310032003a003a0053005300330034003a0000005300200008003100000032003a0033003a0034003a0031003a0032003a0033003a0034003a001300000000000000520008004c0000002000610070006f007200650063006100f300690020006e0065007300630020006d006f00006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000001200000435000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
26000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006c007000740065002000f3006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000070a00000000003400b0000000500000000000000f00000000000033f0b000000080000000000000000000500000000000300000000000000560003000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000009ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000090000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000090000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000900000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000090000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000900000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000903000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
19w_mant_mues_cajasprod_procalm.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
