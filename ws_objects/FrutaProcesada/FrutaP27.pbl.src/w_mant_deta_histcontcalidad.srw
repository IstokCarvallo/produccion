$PBExportHeader$w_mant_deta_histcontcalidad.srw
$PBExportComments$Mantención Detalle Histórico de Control de Calidad.
forward
global type w_mant_deta_histcontcalidad from w_mant_detalle
end type
type cb_detalle from commandbutton within w_mant_deta_histcontcalidad
end type
end forward

global type w_mant_deta_histcontcalidad from w_mant_detalle
integer width = 3250
integer height = 1728
cb_detalle cb_detalle
end type
global w_mant_deta_histcontcalidad w_mant_deta_histcontcalidad

type variables
Date		id_FechaAcceso
Time		it_HoraAcceso


DataWindowChild	idwc_especie, idwc_planta
end variables

forward prototypes
public function boolean duplicado (long al_numero)
public subroutine buscapallet ()
public function boolean noexistepallet (long al_numero)
end prototypes

public function boolean duplicado (long al_numero);Long		ll_fila, ll_numero
Integer	li_cliente, li_planta

//ll_fila	=	dw_1.Find("paen_numero = " + String(al_Numero)+" AND coca_estant = 1" , 1, istr_mant.dw.RowCount())
//
//IF ll_fila > 0 and ll_fila <> il_fila THEN
//	MessageBox("Error","Pallet Ya Fue Incluído en Detalle de Control de Calidad", &
//	Information!, Ok!)
//	RETURN True
//END IF

RETURN False
end function

public subroutine buscapallet ();String	 		ls_Nula
Str_busqueda	lstr_busq

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
lstr_busq.argum[3]	=	"1"
lstr_busq.argum[12]	=	""

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

If lstr_busq.argum[2] <> "" Then
	//NoExistePallet(Long(lstr_busq.Argum[2]))
	If NoExistePallet(Long(lstr_busq.Argum[2])) Or &
			Duplicado(Long(lstr_busq.Argum[2])) Then
		dw_1.SetItem(il_fila, "paen_numero", Long(ls_Nula))
		dw_1.SetColumn("paen_numero")
		dw_1.SetFocus()
	End If
	
	istr_mant.Argumento[4]	=	lstr_busq.Argum[2]
	pb_acepta.SetFocus()	
Else
	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
End If

RETURN
	
end subroutine

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_Cliente, li_tipopa, li_cajas, li_Especie, li_Estado, li_CCalidad, li_Planta, ll_cont

li_Cliente	=	Integer(istr_mant.argumento[1])
li_Planta	=	Integer(istr_mant.argumento[2])

SELECT	pae.paen_tipopa, pae.espe_codigo, var.vari_nombre, pae.emba_codigo,
			pae.paen_ccajas, pae.paen_estado, pae.plde_codigo, IsNull(pae.paen_concal, 1)
	INTO	:li_tipopa, :li_Especie, :ls_nomvar, :ls_embala, 
	      :li_cajas, :li_Estado, :li_planta, :li_CCalidad
	FROM	dbo.palletencab as pae, dbo.variedades as var
	WHERE pae.clie_codigo	= 	:li_Cliente
	AND	pae.paen_numero	= 	:al_Numero
	AND	pae.plde_codigo	=	:li_Planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo;
		
	SELECT count(*)
	INTO :ll_cont
	FROM dbo.palletfruta
	WHERE clie_codigo	= :li_cliente
	AND	paen_numero	= :al_Numero
	AND	plde_codigo	= :li_Planta;
	
If ll_cont = 0 Then
	MessageBox("Atención", "Pallet NO Existe en Definitivo.", Exclamation!, OK!)
	Return True
End If

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	Return  True
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", Exclamation!, OK!)
	Return  True
ElseIf li_Estado = 2 Then
	MessageBox("Atención", "Pallet no está en Existencia, fue Despachado.~r~r" + &
				"Ingrese o seleccione otro Número.", Exclamation!, OK!)
	Return  True
ElseIf li_Estado = 3 Then
	MessageBox("Atención", "Pallet no está en Existencia, fue Repalletizado.~r~r" + &
				"Ingrese o seleccione otro Número.", Exclamation!, OK!)
	Return  True
Else			
	dw_1.SetItem(il_fila, "paen_numero", al_numero)
	dw_1.SetItem(il_fila, "espe_codigo", li_Especie)
	dw_1.SetItem(il_fila, "vari_nombre", ls_nomvar)
	dw_1.SetItem(il_fila, "paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "emba_codigo", ls_embala)
	dw_1.SetItem(il_fila, "paen_ccajas", li_cajas)
	
	If IsNull(li_CCalidad) OR li_CCalidad = 0 Then
		dw_1.Object.coca_estant.Protect	=	0
		dw_1.SetColumn("coca_estant")
		dw_1.SetFocus()		
	Else
		dw_1.Object.coca_estant.Protect	=	1
		
		dw_1.SetItem(il_fila, "coca_estant", li_CCalidad)
		dw_1.SetColumn("coca_estneo")
		dw_1.SetFocus()
	End If	
	
	Return False
End If
end function

on w_mant_deta_histcontcalidad.create
int iCurrent
call super::create
this.cb_detalle=create cb_detalle
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_detalle
end on

on w_mant_deta_histcontcalidad.destroy
call super::destroy
destroy(this.cb_detalle)
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.Modify("buscapallet.Visible = 1")

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])
ias_campo[2]	=	String(dw_1.Object.coca_estneo[il_fila])
ias_campo[3]	=	String(dw_1.Object.coca_observ[il_fila])

IF istr_mant.Agrega THEN
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "coca_fechac", Datetime(istr_mant.argumento[3]))
	
ElseIf Not istr_mant.Agrega And Not istr_Mant.Borra THEN
		dw_1.Object.paen_numero.Protect				=	1
		dw_1.Object.paen_numero.Color					=	RGB(255,255,255)
		dw_1.Object.paen_numero.BackGround.Color	=	553648127
		dw_1.Modify("buscapallet.Visible=0")
END IF

dw_1.Object.coca_estant.Protect				=	1
dw_1.Object.coca_estant.Color 				= RGB(255,255,255)
dw_1.Object.coca_estant.BackGround.Color = RGB(0,0,255)
end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])
dw_1.Object.coca_estneo[il_fila]	=	Integer(ias_campo[2])
dw_1.Object.coca_observ[il_fila]	=	ias_campo[3]



end event

event ue_antesguardar;call super::ue_antesguardar;Integer		li_cont, li_Cliente, li_planta, li_secuen
String			ls_mensaje, ls_colu[]
Datetime	  	ld_fecha
Long 			ll_Pallet

If IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 Then
   li_cont ++
	ls_mensaje 		= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
End If

If IsNull(dw_1.Object.coca_estant[il_fila]) OR dw_1.Object.coca_estant[il_fila] = 0 Then
   li_cont ++
	ls_mensaje 		= ls_mensaje + "~nEstado Anterior"
	ls_colu[li_cont]	= "coca_estant"
End If

If IsNull(dw_1.Object.coca_estneo[il_fila]) OR dw_1.Object.coca_estneo[il_fila] = 0 Then
   li_cont ++
	ls_mensaje 		= ls_mensaje + "~nEstado Actual"
	ls_colu[li_cont]	= "coca_estneo"
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
Else	
	
	li_Cliente	=	dw_1.Object.clie_codigo[il_fila]
	ll_pallet	=	dw_1.Object.paen_numero[il_fila]
	li_planta	=	dw_1.Object.plde_codigo[il_fila]
	ld_fecha	=	dw_1.Object.coca_fechac[il_fila]
	
	SELECT	Max(coca_secuen)
		INTO	:li_secuen
		FROM	dbo.histcontcalidad
		WHERE	clie_codigo =  :li_cliente
		AND	paen_numero	=	:ll_pallet
		AND	plde_codigo	=	:li_planta
		AND	DateDiff(dd, coca_fechac, :ld_fecha) = 0;
	
	If IsNull(li_secuen) Then li_secuen = 0
	
	If IsNull(dw_1.Object.coca_secuen[il_fila]) or dw_1.Object.coca_secuen[il_fila] = 0 Then
		li_secuen ++
		dw_1.Object.coca_secuen[il_fila] = li_secuen
	End If

End If
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))

dw_1.SetItem(il_fila, "coca_fechac", Datetime(Date(Mid(istr_mant.argumento[3], 1, 10)), Now()))
end event

event open;/*
	Argumentos	:	[1]	=	Código de Cliente
						[2]	=	Código de Planta
						[3]	=	Fecha de Control de Calidad
						[5]	=	Hora Grabación
*/
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve()

dw_1.SetTransObject(sqlca)
//istr_mant.dw.ShareData(dw_1)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

end event

event closequery;call super::closequery;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

event resize;call super::resize;cb_detalle.y	=	pb_salir.y + 265
cb_detalle.x	=	pb_salir.x
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_histcontcalidad
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_histcontcalidad
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_histcontcalidad
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_histcontcalidad
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_histcontcalidad
integer x = 2816
integer y = 428
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_histcontcalidad
integer x = 2816
integer y = 212
end type

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_histcontcalidad
integer x = 2816
integer y = 644
end type

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_histcontcalidad
integer x = 73
integer y = 112
integer width = 2629
integer height = 1384
string dataobject = "dw_mant_histocontcalidad"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "paen_numero"
		IF NoExistePallet(Long(data)) OR &
			Duplicado(Long(Data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))			
			RETURN 1
		ELSE
			istr_mant.Argumento[4]	=	data
		END IF
	
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscapallet"
		BuscaPallet()
		
END CHOOSE
end event

type cb_detalle from commandbutton within w_mant_deta_histcontcalidad
integer x = 2757
integer y = 876
integer width = 274
integer height = 108
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Detalle"
end type

event clicked;call super::clicked;/*
	Argumentos de istr_mant2 => Recepción por Ingreso o Consulta
		Argumento	[1]	=	Código de Planta
						[2]	=	Número de Folio Recepción
						[3]	=	Código de Exportador
						[4]	=	Cantidad de Tarjas
						[5]	=	Tipo de packing
						[6]	=	Número de Pallet
						[7]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[8]	=	
						[9]	=	Código de Especie
						[10]	=	Código de Variedad
						[11]	=	
						[12]	=	
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
*/
Str_mant		lstr_mant

IF dw_1.RowCount() > 0 THEN
		lstr_mant.Argumento[2]	=	String(dw_1.GetitemNumber(il_fila, "paen_numero"))
		lstr_mant.Argumento[3]	=	istr_mant.Argumento[1]
		lstr_mant.Argumento[6]	=	String(dw_1.GetitemNumber(il_fila, "paen_numero"))
		lstr_mant.Argumento[21]	=	''
		
	IF lstr_mant.Argumento[2] <> "" THEN
		OpenWithParm(w_maed_palletencab_consulta, lstr_mant)
	END IF
END IF
end event

