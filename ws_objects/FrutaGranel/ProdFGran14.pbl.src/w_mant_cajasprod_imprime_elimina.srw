$PBExportHeader$w_mant_cajasprod_imprime_elimina.srw
$PBExportComments$Ventana de Informe de Control Calidad de Daños y Defectos
forward
global type w_mant_cajasprod_imprime_elimina from w_para_informes
end type
type st_3 from statictext within w_mant_cajasprod_imprime_elimina
end type
type st_4 from statictext within w_mant_cajasprod_imprime_elimina
end type
type pb_1 from picturebutton within w_mant_cajasprod_imprime_elimina
end type
type rb_2 from radiobutton within w_mant_cajasprod_imprime_elimina
end type
type gb_3 from groupbox within w_mant_cajasprod_imprime_elimina
end type
type rb_1 from radiobutton within w_mant_cajasprod_imprime_elimina
end type
type dw_lotes from datawindow within w_mant_cajasprod_imprime_elimina
end type
type st_2 from statictext within w_mant_cajasprod_imprime_elimina
end type
type em_recepcion from editmask within w_mant_cajasprod_imprime_elimina
end type
type dw_1 from datawindow within w_mant_cajasprod_imprime_elimina
end type
type st_6 from statictext within w_mant_cajasprod_imprime_elimina
end type
type uo_selmercado from uo_seleccion_mercados within w_mant_cajasprod_imprime_elimina
end type
type st_7 from statictext within w_mant_cajasprod_imprime_elimina
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_cajasprod_imprime_elimina
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_cajasprod_imprime_elimina
end type
type sle_camara from singlelineedit within w_mant_cajasprod_imprime_elimina
end type
type ddlb_filtro from dropdownlistbox within w_mant_cajasprod_imprime_elimina
end type
type dw_2 from uo_dw within w_mant_cajasprod_imprime_elimina
end type
end forward

global type w_mant_cajasprod_imprime_elimina from w_para_informes
integer width = 4416
integer height = 1956
event ue_recuperadatos ( )
event ue_imprimir ( )
event ue_guardar ( )
event ue_antesguardar ( )
st_3 st_3
st_4 st_4
pb_1 pb_1
rb_2 rb_2
gb_3 gb_3
rb_1 rb_1
dw_lotes dw_lotes
st_2 st_2
em_recepcion em_recepcion
dw_1 dw_1
st_6 st_6
uo_selmercado uo_selmercado
st_7 st_7
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
sle_camara sle_camara
ddlb_filtro ddlb_filtro
dw_2 dw_2
end type
global w_mant_cajasprod_imprime_elimina w_mant_cajasprod_imprime_elimina

type variables
uo_correlcompequipo		iuo_Equipo

uo_lotescorrelequipo_gr	iuo_correl
uo_cliente					iuo_clie
uo_spro_ordenproceso	iuo_spro_ordenproceso
uo_spro_palletencab		iuo_Pallet

str_busqueda				istr_busq
str_mant						istr_mant

Integer						ii_Parametro, ii_sdprusia
Long							il_Inicio[], il_Final[]
String							is_formatopomaceas


Boolean						ib_impresora
uo_manejoimpresora		iuo_impresora
uo_voicecode				iuo_voicecode
uo_QR						iuo_QR
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean noexisteplanta (integer planta, integer cliente)
public subroutine obtienesegmentos ()
public function boolean wf_actualiza_db ()
end prototypes

event ue_recuperadatos();Datawindowchild  ldwc_lotes
Long	ll_fila, respuesta

DO
	dw_lotes.GetChild("vari_codigo", ldwc_lotes)
	ldwc_lotes.SetTransObject(SqlCa)
	ldwc_lotes.Retrieve(0)
	
	dw_lotes.SetTransObject(Sqlca)
	ll_fila	= dw_lotes.Retrieve(uo_SelPlanta.Codigo, Dec(em_recepcion.Text), uo_SelCliente.Codigo)

	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)

	ElseIf ll_fila = 0 Then
		Messagebox("Error","No Existe Pallet Para Esta Planta")
	Else
		pb_acepta.Enabled 	= 	True
		gb_3.Enabled 			=	True
		rb_1.Enabled 			=	True
		rb_2.Enabled 			=	True
		If ii_Parametro = 1 Then
			DO
				ll_fila	= dw_2.Retrieve(uo_SelPlanta.Codigo, long(em_recepcion.Text), uo_SelCliente.Codigo)

				If ll_fila = -1 Then
					respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
													Information!, RetryCancel!)
				End If
			LOOP WHILE respuesta = 1
		End If
	End If									
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_imprimir();Long		ll_Fila, ll_fila_alter, ll_filafinal
Integer	li_sdprusia, li_resta=0
String		ls_fecha, ls_codigo, ls_CSG, ls_Embalaje, ls_Calibre, ls_QR, ls_Ruta, ls_Productor
Boolean	lb_siguiente

SetPointer(HourGlass!)

If Not iuo_Equipo.ObtieneEspecie(uo_SelCliente.Codigo, Long(em_recepcion.Text), True, Sqlca) Then Return
If Not iuo_Equipo.ObtieneFormato(uo_SelPlanta.Codigo, gstr_us.Computador, True, Sqlca) Then Return

iuo_clie.Existe(uo_selcliente.codigo, False, sqlca)
iuo_correl.ExisteCorrel(uo_selplanta.codigo, 99, gstr_us.computador, FALSE, sqlca)

dw_1.dataObject	=	iuo_Equipo.Formato
dw_1.SetTransObject(SQLCA)
dw_1.Reset()

If dw_lotes.RowCount() > 0 Then
	If dw_lotes.Object.espe_codigo[1] = 21 AND gstr_apl.CodigoSistema	<> 27 Then  li_resta = 1
End If

For ll_fila = 1 TO dw_lotes.RowCount()
	If dw_lotes.IsSelected(ll_fila) Then
		If iuo_correl.foad_canocx > (1 + li_resta)  Then
			FOR ll_filafinal = ll_fila + 1 TO dw_lotes.RowCount()
				If dw_lotes.IsSelected(ll_filafinal) Then
					Exit
				End If
			NEXT
			If ll_filafinal > dw_lotes.RowCount() Then
				ll_filafinal = ll_fila
			Else
				If dw_lotes.Object.prod_codigo[ll_fila] <> dw_lotes.Object.prod_codigo[ll_filafinal] Then
					ll_filafinal = ll_fila
				End If
			End If
		End If
		
		If ll_filafinal = 0 Then
			ll_filafinal = ll_fila
		End If
		
		If uo_SelCliente.Codigo <> 81 And  uo_SelCliente.Codigo <> 15 Then
			dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo,   &
							  dw_lotes.object.pafr_secuen[ll_fila], dw_lotes.object.pafr_secuen[ll_fila], &
							  1, &
							  sle_camara.Text, ii_sdprusia)
			
			If iuo_correl.foad_canocx > (1+ li_resta) Then
				dw_1.Retrieve(uo_SelCliente.Codigo,  uo_SelPlanta.Codigo,   &
								  dw_lotes.object.pafr_secuen[ll_filafinal], dw_lotes.object.pafr_secuen[ll_filafinal], &
								  1, &
								  sle_camara.Text, ii_sdprusia)
			End If
			
		Else
			dw_1.Retrieve(uo_SelCliente.Codigo,  uo_SelPlanta.Codigo, &
							  dw_lotes.object.pafr_secuen[ll_fila], dw_lotes.object.pafr_secuen[ll_fila],  uo_SelMercado.Codigo)
							  
			If iuo_correl.foad_canocx > ( 1+ li_resta) Then
				dw_1.Retrieve(uo_SelCliente.Codigo,  uo_SelPlanta.Codigo, &
								  dw_lotes.object.pafr_secuen[ll_filafinal], dw_lotes.object.pafr_secuen[ll_filafinal], uo_SelMercado.Codigo)
			End If
			
		End If
		dw_lotes.SelectRow(ll_fila, False)
		dw_lotes.SelectRow(ll_filafinal, False)
		
		If dw_1.RowCount() < 1 Then
			MessageBox("Problema de Datos",  "El registro de la caja ha sido creado, pero no es posible generar el compacto."+&
														"~r~nFavor comunicar situación al Encargado del Sistema.", Exclamation!)
		Else
			If dw_1.RowCount() > 0 Then

				If IsNull(dw_1.Object.prpr_prepro[1]) Or dw_1.Object.prpr_prepro[1] = "" Then 
					ls_CSG = "0000000"
				Else
					ls_CSG	= Fill('0', 7 - Len(String(dw_1.Object.prpr_prepro[1]))) + dw_1.Object.prpr_prepro[1]
				End If
				
				ls_fecha			=	String(dw_1.Object.capr_fecemb[1])
				ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)

				ls_codigo			=	"01" + dw_1.Object.emba_nroint[1] + "13" + ls_fecha //+ "\F"
				ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  //+ "\F"
			//	ls_codigo			=	ls_codigo	+	"21" + String(dw_1.Object.plde_codigo[1], "0000") 
			//	ls_codigo			=	ls_codigo	+	String(dw_1.Object.capr_numero[1], '00000000')	
				
				/*
				Codigo QR
				*/		
				ls_Embalaje = dw_1.Object.emba_codigo[1]
				ls_Calibre	= dw_1.Object.capr_calibr[1]
				ls_Productor= dw_1.Object.prod_codigo[1]
				
				ls_QR = String(dw_1.Object.plde_codigo[1], "0000")	+	String(dw_1.Object.capr_numero[1], '00000000')	+ Char(10)					
				ls_QR	+= 'Prod:' + ls_Productor + Char(10) 
				ls_QR	+= 'Esp:' + String(dw_1.Object.espe_codigo[1], '00') + ' / ' + String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
				ls_QR	+= 'Pre:' + ls_CSG + ' / ' + String(dw_1.Object.prod_cuarte[1],'000' )  + Char(10)
				ls_QR	+= 'Emb:' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
				ls_QR	+= String(dw_1.Object.capr_fecemb[1], 'dd/mm/yyyy')
				
				ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
				dw_1.Object.p_qrcode.FileName = ls_Ruta
	
				dw_1.Object.Ole_1.Object.BarCode		=	iuo_clie.Barras
				dw_1.Object.Ole_1.Object.Text 			= 	ls_codigo
				
				//Codigo UPC
				dw_1.Object.Ole_UPC.Object.Text 		= 	dw_1.Object.gtin_codupc[1]
				
				iuo_voicecode	=	Create uo_voicecode
				iuo_voicecode.voicecode(dw_1	, dw_1.Object.emba_nroint[1],  ls_fecha, dw_1.Object.capr_fecemb[1], iuo_correl.foad_vopico)

				If iuo_correl.foad_canocx = (2 -  li_resta)  AND dw_1.RowCount() > 1 Then
					ls_fecha			=	String(dw_1.Object.capr_fecemb[2])
					ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)

					If IsNull(dw_1.Object.prpr_prepro[1]) Or dw_1.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_1.Object.prpr_prepro[1]))) + dw_1.Object.prpr_prepro[1]
					End If

					ls_codigo			=	"01" + dw_1.Object.emba_nroint[2] + "13" + ls_fecha //+ "\F"
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  //+ "\F"
				//	ls_codigo			=	ls_codigo	+	"21" + String(dw_1.Object.plde_codigo[2], "0000") 
				//	ls_codigo			=	ls_codigo	+	String(dw_1.Object.capr_numero[2], '00000000')
						
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_1.Object.emba_codigo[1]
					ls_Calibre	= dw_1.Object.capr_calibr[1]
					ls_Productor= dw_1.Object.prod_codigo[1]
					
					ls_QR = String(dw_1.Object.plde_codigo[2], "0000")	+	String(dw_1.Object.capr_numero[2], '00000000')	+ Char(10)
					ls_QR	+= 'Prod:' + ls_Productor  + Char(10) 
					ls_QR	+= 'Esp:' + String(dw_1.Object.espe_codigo[1], '00') + ' / ' + String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Pre:' + ls_CSG + ' / ' + String(dw_1.Object.prod_cuarte[1],'000' )  + Char(10)
					ls_QR	+= 'Emb:' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= String(dw_1.Object.capr_fecemb[1], 'dd/mm/yyyy')
					
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_1.Object.p_qrcode.FileName = ls_Ruta
	
					dw_1.Object.Ole_2.Object.BarCode		=	iuo_clie.Barras
					If ll_filafinal <> ll_fila Then
						dw_1.Object.Ole_2.Object.Text 		= 	ls_codigo
					Else
						dw_1.Object.Ole_2.Object.Text 		= 	''
					End If
				End If
	
				If iuo_correl.foad_canocx = (3 - li_resta)   AND dw_1.RowCount() > 2 Then
					ls_fecha			=	String(dw_1.Object.capr_fecemb[3])
					ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					
					If IsNull(dw_1.Object.prpr_prepro[1]) Or dw_1.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_1.Object.prpr_prepro[1]))) + dw_1.Object.prpr_prepro[1]
					End If

					ls_codigo			=	"01" + dw_1.Object.emba_nroint[3] + "13" + ls_fecha //+ "\F"
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  //+ "\F"
				//	ls_codigo			=	ls_codigo	+	"21" + String(dw_1.Object.plde_codigo[3], "0000") 
				//	ls_codigo			=	ls_codigo	+	String(dw_1.Object.capr_numero[3], '00000000')
						
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_1.Object.emba_codigo[1]
					ls_Calibre	= dw_1.Object.capr_calibr[1]
					ls_Productor= dw_1.Object.prod_codigo[1]
					
					ls_QR = String(dw_1.Object.plde_codigo[3], "0000")	+	String(dw_1.Object.capr_numero[3], '00000000')	+ Char(10)					
					ls_QR	+= 'Prod:' + ls_Productor + Char(10) 
					ls_QR	+= 'Esp:' + String(dw_1.Object.espe_codigo[1], '00') + ' / ' + String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Pre:' + ls_CSG + ' / ' + String(dw_1.Object.prod_cuarte[1],'000' )  + Char(10)
					ls_QR	+= 'Emb:' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= String(dw_1.Object.capr_fecemb[1], 'dd/mm/yyyy')
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_1.Object.p_qrcode.FileName = ls_Ruta
					
					dw_1.Object.Ole_3.Object.BarCode	=	iuo_clie.Barras
					dw_1.Object.Ole_3.Object.Text 		= 	ls_codigo
					
					//Codigo UPC
//					dw_1.Object.Ole_UPC.Object.BarCode		=	iuo_clie.Barras
					dw_1.Object.Ole_UPC.Object.Text 			= 	dw_1.Object.gtin_codupc[1]
				End If
							
				If IsNull(iuo_Equipo.Impresora) Or Len(iuo_Equipo.Impresora) < 1 Then
					ib_impresora	=	False
				Else
					iuo_impresora.asignaimpresora_comp(iuo_Equipo.Impresora)
					ib_impresora	=	True
				End If
				
				If iuo_impresora.is_impresoracomp <> '' AND ib_impresora Then
					iuo_impresora.setimprcomp()
					
					dw_1.AcceptText()
					dw_1.Print()
					dw_1.Reset()
					
					iuo_impresora.setimprdef()
				Else
					dw_1.AcceptText()
					dw_1.Print()
					dw_1.Reset()
				End If
			End If
		End If	
	End If
Next

dw_1.Reset()

SetPointer(Arrow!)

end event

event ue_guardar();IF dw_lotes.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar();Long	ll_fila = 1

DO WHILE ll_fila <= dw_lotes.RowCount()
	IF dw_lotes.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_lotes.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP
end event

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
	INTO		:ls_nombre
	FROM 	dbo.clientesprod
	WHERE	clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean noexisteplanta (integer planta, integer cliente);Integer	codigo

	
	SELECT plde_codigo
	INTO	 :Codigo	
	FROM	dbo.Plantadesp
	WHERE	plde_codigo	=	:planta;
	
	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas" )
			Return True			
	ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
			Return True						
	END IF

	Return False


end function

public subroutine obtienesegmentos ();Integer	li_filas, li_segmentos

li_segmentos = 1

FOR li_filas =  1 TO dw_lotes.RowCount()
	
	IF dw_Lotes.IsSelected(li_filas) THEN
		
		IF li_filas = 1 THEN 
			il_inicio[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas]
		ELSEIF li_filas = dw_lotes.RowCount() THEN 
			il_final[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas]
			li_segmentos ++
		ELSEIF dw_Lotes.IsSelected(li_filas - 1) = FALSE THEN
			il_inicio[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas]
		END IF
	
	ELSE
		
		IF li_filas > 1 THEN
			IF dw_Lotes.IsSelected(li_filas - 1) THEN
				il_final[li_segmentos]	=	dw_lotes.Object.pafr_secuen[li_filas - 1]
				li_segmentos ++
			END IF
		END IF
		
	END IF
	
NEXT
end subroutine

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_lotes.Update(True, False) = 1 then 
	IF dw_2.Update(True, False) = 1 then 
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		ELSE
			lb_Retorno	=	True
				
			dw_lotes.ResetUpdate()
			dw_2.ResetUpdate()
		END IF
	ELSE
		RollBack;
		
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_mant_cajasprod_imprime_elimina.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.pb_1=create pb_1
this.rb_2=create rb_2
this.gb_3=create gb_3
this.rb_1=create rb_1
this.dw_lotes=create dw_lotes
this.st_2=create st_2
this.em_recepcion=create em_recepcion
this.dw_1=create dw_1
this.st_6=create st_6
this.uo_selmercado=create uo_selmercado
this.st_7=create st_7
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.sle_camara=create sle_camara
this.ddlb_filtro=create ddlb_filtro
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.pb_1
this.Control[iCurrent+4]=this.rb_2
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.rb_1
this.Control[iCurrent+7]=this.dw_lotes
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.em_recepcion
this.Control[iCurrent+10]=this.dw_1
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.uo_selmercado
this.Control[iCurrent+13]=this.st_7
this.Control[iCurrent+14]=this.uo_selplanta
this.Control[iCurrent+15]=this.uo_selcliente
this.Control[iCurrent+16]=this.sle_camara
this.Control[iCurrent+17]=this.ddlb_filtro
this.Control[iCurrent+18]=this.dw_2
end on

on w_mant_cajasprod_imprime_elimina.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.pb_1)
destroy(this.rb_2)
destroy(this.gb_3)
destroy(this.rb_1)
destroy(this.dw_lotes)
destroy(this.st_2)
destroy(this.em_recepcion)
destroy(this.dw_1)
destroy(this.st_6)
destroy(this.uo_selmercado)
destroy(this.st_7)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.sle_camara)
destroy(this.ddlb_filtro)
destroy(this.dw_2)
end on

event open;Boolean	lb_Cerrar

If IsNull(uo_SelMercado.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_Selplanta.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar =	True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Equipo					=	Create uo_correlcompequipo
	iuo_correl					=	Create uo_lotescorrelequipo_gr
	iuo_clie						=	Create uo_cliente
	iuo_impresora				=	Create uo_manejoimpresora
	iuo_spro_ordenproceso	=	Create uo_spro_ordenproceso
	iuo_Pallet					=	Create uo_spro_palletencab
	iuo_QR						=	Create uo_QR
	
	uo_SelMercado.Seleccion(False,False)
	uo_SelPlanta.Seleccion(False,False)
	uo_SelCliente.Seleccion(False,False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	uo_SelCliente.Inicia(gi_CodExport)

	dw_2.SetTransObject(SQLCa)
	
	sle_camara.Visible			=	False
	st_7.Text						=	"Mercado"
	uo_selmercado.Visible	=	True
	ddlb_filtro.Visible			=	False
		
	ii_Parametro 	=	Integer(Message.StringParm)

	If ii_Parametro = 1 Then
		This .Title 						=	'ELIMINACIÓN DE CAJAS EMBALADAS'
		st_titulo.Text					=	'ELIMINACIÓN DE CAJAS EMBALADAS'
		pb_acepta.PictureName		=	'\Desarrollo 17\Imagenes\Botones\Borrar.png'
		pb_acepta.DisabledName	=	'\Desarrollo 17\Imagenes\Botones\Borrar-bn.png'
	Else
		THIS.Title 						= 	'RE IMPRESIÓN DE CAJAS EMBALADAS'
		ST_TITULO.Text				=	'RE IMPRESIÓN DE CAJAS EMBALADAS'
		pb_acepta.PictureName		=	'\Desarrollo 17\Imagenes\Botones\Imprimir.png'
		pb_acepta.DisabledName	=	'\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png'
	End If

	pb_acepta.Enabled 				=	False
	
End If
end event

event resize;Integer		li_posi_y, li_objeto

pb_acepta.x			=	This.WorkSpaceWidth() - 350
pb_acepta.y			=	300
pb_acepta.width	=	300
pb_acepta.height	=	245

pb_salir.x			=	This.WorkSpaceWidth() - 350
pb_salir.y			=	650
pb_salir.width		=	300
pb_salir.height		=	245

st_titulo.Width		=	This.Width - 550
st_2.Width			=	This.Width - 550

dw_lotes.x			=	st_2.x
dw_lotes.y			=	st_2.y + st_2.Height + 10
dw_lotes.Width		=	st_2.Width
dw_lotes.Height	=	This.Height - 870

sle_camara.x	=	uo_SelMercado.x
sle_camara.y	=	uo_SelMercado.y
end event

type pb_excel from w_para_informes`pb_excel within w_mant_cajasprod_imprime_elimina
integer x = 3470
integer y = 600
end type

type st_computador from w_para_informes`st_computador within w_mant_cajasprod_imprime_elimina
boolean visible = false
integer x = 3406
integer y = 216
end type

type st_usuario from w_para_informes`st_usuario within w_mant_cajasprod_imprime_elimina
boolean visible = false
integer x = 3406
integer y = 160
integer height = 68
end type

type st_temporada from w_para_informes`st_temporada within w_mant_cajasprod_imprime_elimina
boolean visible = false
integer x = 3406
integer y = 76
end type

type p_logo from w_para_informes`p_logo within w_mant_cajasprod_imprime_elimina
boolean visible = false
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_mant_cajasprod_imprime_elimina
integer x = 87
integer y = 60
integer width = 3227
string text = "Re-Impresión de Compactos."
end type

type pb_acepta from w_para_informes`pb_acepta within w_mant_cajasprod_imprime_elimina
integer x = 3483
integer y = 1148
integer taborder = 70
end type

event pb_acepta::clicked;Integer li_fila, li_caja

If uo_SelCliente.Codigo = 81 THEN
	If uo_SelMercado.Codigo = -1 Or IsNull(uo_SelMercado.Codigo) Then
		MessageBox('Atencion', 'Debe Seleccionar un Mercado.', StopSign!, Ok!)
		Return
	End If
End If

If ii_Parametro = 1 Then	
	FOR li_fila = dw_lotes.RowCount() TO 1 STEP -1
		If dw_lotes.IsSelected(li_fila) Then 
				If iuo_spro_ordenproceso.Existe(dw_lotes.Object.plde_codigo[li_fila], dw_lotes.Object.pafr_tipdoc[li_fila], &
												  dw_lotes.Object.pafr_docrel[li_fila], True, SqlCa, uo_SelCliente.Codigo) Then			  
						If iuo_spro_ordenproceso.Estado = 5 Then
							MessageBox("Atención", "Cajas N° : "+   String(dw_lotes.Object.pafr_secuen[li_fila]) + " corresponde a un proceso con Cierre Web,~n~n " + &
											"No Puede ser Eliminada",Exclamation!) 
						Else
							li_caja	=	dw_2.Find("capr_numero = " + String(dw_lotes.Object.pafr_secuen[li_fila]), 1, dw_2.RowCount())
							If li_caja > 0 Then
								dw_2.Object.capr_estado[li_caja] = 3
							End If
							dw_lotes.DeleteRow(li_fila)
						End If
				End If
		End If		
	NEXT
	Parent.TriggerEvent("ue_guardar")
Else
	Parent.TriggerEvent("ue_imprimir")
End If
end event

type pb_salir from w_para_informes`pb_salir within w_mant_cajasprod_imprime_elimina
integer x = 3479
integer y = 1512
integer taborder = 80
end type

type st_3 from statictext within w_mant_cajasprod_imprime_elimina
integer x = 192
integer y = 364
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_cajasprod_imprime_elimina
integer x = 192
integer y = 488
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
string text = "N° de Pallet"
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_mant_cajasprod_imprime_elimina
integer x = 1051
integer y = 468
integer width = 123
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
boolean originalsize = true
end type

event clicked;Long							ll_fila
String 						ls_Null
Str_Busqueda				lstr_Busq
uo_spro_palletencab		luo_spro_palletencab

SetNull(ls_Null)

lstr_Busq.Argum[1] =	String(uo_SelPlanta.Codigo)
lstr_Busq.Argum[2] = "0"
lstr_Busq.Argum[3] =	"0"
lstr_Busq.Argum[4] = "0"//istr_Mant.Argumento[8]
lstr_Busq.Argum[5] =	String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_pallet_movimiento, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) > 2 Then
	If lstr_Busq.Argum[2] <> "" Then
		em_recepcion.Text	=	lstr_Busq.Argum[2]
		If iuo_Pallet.Existe(uo_SelCliente.Codigo,  uo_SelPlanta.Codigo, &
												 Long(lstr_Busq.Argum[2]),False,SqlCa) Then
			If ii_Parametro = 1 Then
				If iuo_Pallet.Estado = 1 Then
					Parent.TriggerEvent("ue_recuperadatos")
					
					For ll_fila	=	1 TO dw_lotes.RowCount()
						dw_lotes.SelectRow(ll_fila, True)
					Next
				
					If ll_fila	>	1 Then rb_1.Checked = TRUE
				Else
					MessageBox('Atencion', 'Folio : ' + This.Text + ' Despachado, no se puede eliminar cajas.', Information!, OK!)
					em_recepcion.Text	=	ls_Null
				End If
			Else
				Parent.TriggerEvent("ue_recuperadatos")
					
				For ll_fila	=	1 TO dw_lotes.RowCount()
					dw_lotes.SelectRow(ll_fila, True)
				Next
			
				If ll_fila	>	1 Then rb_1.Checked = TRUE
			End If
		Else
			MessageBox("Error de Datos","El pallet ingresado pertenece a otra especie.")
			em_recepcion.Text	=	ls_Null
			Return
		End If
	End If
End If
end event

type rb_2 from radiobutton within w_mant_cajasprod_imprime_elimina
integer x = 1952
integer y = 444
integer width = 622
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Desaplicar Todos"
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, False)
NEXT 
end event

type gb_3 from groupbox within w_mant_cajasprod_imprime_elimina
integer x = 1842
integer y = 356
integer width = 1445
integer height = 216
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Opciones"
borderstyle borderstyle = styleraised!
end type

type rb_1 from radiobutton within w_mant_cajasprod_imprime_elimina
integer x = 2615
integer y = 444
integer width = 622
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Aplicar Todos"
boolean checked = true
end type

event clicked;Long	ll_fila
FOR ll_fila	=	1 TO dw_lotes.RowCount()
	dw_lotes.SelectRow(ll_fila, True)
NEXT 
end event

type dw_lotes from datawindow within w_mant_cajasprod_imprime_elimina
integer x = 137
integer y = 660
integer width = 3154
integer height = 972
boolean bringtotop = true
boolean titlebar = true
string title = "CAJAS"
string dataobject = "dw_mues_palletfruta_elimina_imprime"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event clicked;IF row = 0 THEN RETURN

IF IsSelected(Row) THEN
	SelectRow (Row, FALSE)
ELSE
	SelectRow (Row, TRUE)
END IF


end event

type st_2 from statictext within w_mant_cajasprod_imprime_elimina
integer x = 87
integer y = 184
integer width = 3227
integer height = 420
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

type em_recepcion from editmask within w_mant_cajasprod_imprime_elimina
integer x = 672
integer y = 480
integer width = 370
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;long ll_fila
istr_busq.Argum[1]	=	' '
istr_busq.Argum[2]	=  ' '
istr_busq.Argum[3]	=  ' '
istr_busq.Argum[5]	=  ' '

istr_busq.Argum[1]	=	String(uo_SelPlanta.Codigo)
istr_busq.Argum[2]	=  '1'
istr_busq.Argum[3]	=  This.Text
istr_busq.Argum[5]	=  '0'
istr_busq.Argum[4]	=  String(uo_SelCliente.Codigo)

If This.Text = "" OR IsNull(This.Text) Then Return 

If  iuo_Pallet.Existe(uo_SelCliente.Codigo,  uo_SelPlanta.Codigo, Long(This.Text), True, SqlCa) Then
	If ii_Parametro = 1 Then 
		If iuo_Pallet.Estado = 1 Then 
			Parent.TriggerEvent("ue_recuperadatos")
		
			FOR ll_fila	=	1 TO dw_lotes.RowCount()
				dw_lotes.SelectRow(0, TRUE)
			NEXT 
			
			If ll_fila	>	1 Then rb_1.Checked = True
		Else
			MessageBox('Atencion', 'Folio : ' + This.Text + ' Despachado, no se puede eliminar cajas.', Information!, OK!)
			This.Text = ''
		End If
	Else
		Parent.TriggerEvent("ue_recuperadatos")		
		FOR ll_fila	=	1 TO dw_lotes.RowCount()
			dw_lotes.SelectRow(0, TRUE)
		NEXT 
		
		If ll_fila	>	1 Then rb_1.Checked = True
	End If
End If
end event

type dw_1 from datawindow within w_mant_cajasprod_imprime_elimina
boolean visible = false
integer x = 3886
integer y = 492
integer width = 393
integer height = 296
integer taborder = 50
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event retrievestart;Return 2
end event

type st_6 from statictext within w_mant_cajasprod_imprime_elimina
integer x = 192
integer y = 252
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
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selmercado from uo_seleccion_mercados within w_mant_cajasprod_imprime_elimina
integer x = 2249
integer y = 236
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selmercado.destroy
call uo_seleccion_mercados::destroy
end on

type st_7 from statictext within w_mant_cajasprod_imprime_elimina
integer x = 1838
integer y = 244
integer width = 293
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

type uo_selplanta from uo_seleccion_plantas within w_mant_cajasprod_imprime_elimina
event destroy ( )
integer x = 672
integer y = 356
integer height = 84
integer taborder = 70
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_cajasprod_imprime_elimina
event destroy ( )
integer x = 672
integer y = 244
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;IF This.Codigo = 81 THEN
	sle_camara.Visible		=	False
	st_7.Text					=	"Mercado"
	uo_SelMercado.Visible	=	True
ELSE
	sle_camara.Visible		=	True
	st_7.Text					=	"Camara"
	uo_SelMercado.Visible	=	False
END IF
end event

type sle_camara from singlelineedit within w_mant_cajasprod_imprime_elimina
boolean visible = false
integer x = 2318
integer y = 316
integer width = 686
integer height = 100
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type ddlb_filtro from dropdownlistbox within w_mant_cajasprod_imprime_elimina
integer x = 2848
integer y = 232
integer width = 288
integer height = 400
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"SDP","SAG"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
	CASE 1 
		ii_sdprusia	=	1
		
	CASE 2
		ii_sdprusia = 0
		
END CHOOSE
end event

type dw_2 from uo_dw within w_mant_cajasprod_imprime_elimina
boolean visible = false
integer x = 3895
integer y = 824
integer width = 379
integer height = 220
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_palletfruta_cajasprod_elimina_imprime"
boolean vscrollbar = false
boolean livescroll = true
borderstyle borderstyle = styleraised!
end type

event clicked;call super::clicked;IF row = 0 THEN RETURN

IF IsSelected(Row) THEN
	SelectRow (Row, FALSE)
ELSE
	SelectRow (Row, TRUE)
END IF


end event

