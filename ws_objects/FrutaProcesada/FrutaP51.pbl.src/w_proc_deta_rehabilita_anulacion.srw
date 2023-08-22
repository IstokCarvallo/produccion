$PBExportHeader$w_proc_deta_rehabilita_anulacion.srw
forward
global type w_proc_deta_rehabilita_anulacion from w_mant_detalle
end type
type cb_1 from commandbutton within w_proc_deta_rehabilita_anulacion
end type
end forward

global type w_proc_deta_rehabilita_anulacion from w_mant_detalle
integer width = 3163
integer height = 1112
cb_1 cb_1
end type
global w_proc_deta_rehabilita_anulacion w_proc_deta_rehabilita_anulacion

type variables
//Str_mant Istr_mant
end variables

forward prototypes
public function boolean duplicado (string campo)
public function boolean existepallet (string ls_columna)
public subroutine buscapallet ()
public function boolean noexistepallet (string ls_columna)
protected function boolean existeinspeccion (long numero, integer cliente, integer planta)
end prototypes

public function boolean duplicado (string campo);Long		ll_fila

ll_fila	= dw_1.Find("paen_numero = " + campo + " ", 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_FilaAnc THEN
	MessageBox("Error","Pallet ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existepallet (string ls_columna);Integer	li_cliente, li_tipopa, li_planta
Long		ll_numero

li_cliente	= 	Integer(istr_mant.argumento[3])
li_planta	=	Integer(istr_mant.argumento[1])
ll_numero 	= 	Long(ls_columna)

SELECT	pae.paen_tipopa
	INTO	:li_tipopa
	FROM	dbo.palletencab as pae
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:ll_numero
	AND	pae.plde_codigo	=	:li_planta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 0 THEN
	MessageBox("Atención","Pallet fue Ingresado Anteriormente.")
	RETURN True	
ELSE
	RETURN False
END IF
	
end function

public subroutine buscapallet ();Str_busqueda	lstr_busq

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 5")

lstr_busq.Argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
lstr_busq.Argum[12]	=	String(dw_1.Object.plde_codigo[il_fila])
lstr_busq.argum[3]	=	"1"
lstr_busq.argum[4]	=	"1"
lstr_busq.argum[6]	=	"2"

OpenWithParm(w_busc_palletencab, lstr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.Argum[2] <> "" THEN
	dw_1.SetItem(il_fila, "paen_numero", Long(istr_busq.Argum[2]))
	
	IF NoExistePallet(istr_busq.Argum[2]) OR Duplicado(istr_busq.Argum[2]) THEN
		dw_1.SetColumn("paen_numero")
		dw_1.SetFocus()
	ELSE		
		pb_acepta.SetFocus()
	END IF
ELSE	
	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

public function boolean noexistepallet (string ls_columna);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_status, li_tipopa, li_Estado, li_planta, li_inspec
Long		ll_numero, ll_cajas, ll_numinpe, ll_numinpe2

li_cliente	= 	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])
ll_numinpe 	= 	Long(istr_mant.argumento[5])
ll_numero	=  Long(ls_columna)
	
SELECT 	pal.paen_tipopa, var.vari_nombre, pal.emba_codigo, pal.cate_codigo, 
			pal.paen_inspec, pal.inpe_numero, pal.stat_codigo, pal.paen_ccajas, pal.paen_estado 
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, 
			:li_inspec,	:ll_numinpe2, :li_status, :ll_cajas, :li_Estado	
   FROM dbo.inspecpaldet as ins,dbo.palletencab as pal,dbo.variedades  as var
   WHERE pal.clie_codigo = ins.clie_codigo  AND  
         pal.paen_numero = ins.paen_numero  AND  
         pal.plde_codigo = ins.plde_codigo  AND
			ins.inpe_numero = :ll_numinpe		  AND
         var.espe_codigo = pal.espe_codigo  AND  
         var.vari_codigo = pal.vari_codigo  AND
			pal.clie_codigo =	:li_cliente 	  AND
			pal.paen_numero =	:ll_numero 		  AND
			pal.plde_codigo =	:li_planta 		  AND
			var.espe_codigo =	pal.espe_codigo  AND
			var.vari_codigo =	pal.vari_codigo  AND
			isnull(inpd_nroanu,0) > 0          AND
			pal.paen_estado = 1;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet No Ha sido Anulado Previamente, Ingrese otro Código.", &
					Exclamation!, OK!)
	RETURN True
ELSEIF li_Estado = 2 THEN
	MessageBox("Atención","Pallet NO Disponible, Fue Despachado.")
	RETURN True
ELSEIF li_Estado = 3 THEN
	MessageBox("Atención","Pallet NO Disponible, ya Fue Repalletizado.")
	RETURN True
ELSEIF li_inspec > 0 AND (ll_numinpe<>ll_numinpe2) THEN
	MessageBox("Atención","Pallet NO Disponible, Fue Inspeccionado Nuevamente en "+String(ll_numinpe2)+" .")
	RETURN True
ELSE
	dw_1.SetItem(il_fila, "paen_numero", ll_numero)
	dw_1.SetItem(il_fila, "vari_nombre", ls_nomvar)
	dw_1.SetItem(il_fila, "paen_tipopa", li_tipopa)
	dw_1.SetItem(il_fila, "emba_codigo", ls_embala)
	dw_1.SetItem(il_fila, "cate_codigo", li_catego)
	dw_1.SetItem(il_fila, "stat_codigo", li_status)
	dw_1.SetItem(il_fila, "paen_inspec", li_inspec)
	dw_1.SetItem(il_fila, "paen_ccajas", ll_cajas)
	RETURN False
END IF
end function

protected function boolean existeinspeccion (long numero, integer cliente, integer planta);Long		ll_numero, ll_anulacion, ll_count
Integer	li_planta, li_cliente, li_tipoin, li_destino
Date		ld_frecha, ld_fechai

li_cliente	=	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])

SELECT DISTINCT inpe_numero
INTO   :numero
FROM dbo.inspecpaldet
WHERE inpe_numero = :numero
AND   clie_codigo = :li_cliente
AND   plde_codigo = :li_planta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Tabla de Inspección")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Inspección NO Existe.~rIngrese otro.", + &
	Exclamation!, OK!)
	Return True
END IF	

SELECT Count(*)
INTO   :ll_count
FROM dbo.inspecpaldet
WHERE inpe_numero = :numero
AND   clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND	isnull(inpd_nroanu,0) > 0;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Tabla de Inspección")
	Return True

ELSEIF ll_count = 0 THEN
	MessageBox("Atención", "Número de Inspección se encuentra Sin Anulaciones SAG.~rIngrese otro.", + &
	Exclamation!, OK!)
	Return True
END IF

SELECT Count(*)
INTO   :ll_count
FROM dbo.inspecpalenc
WHERE inpe_numero = :numero
AND   clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND	isnull(inpe_estado,0) > 1
AND	isnull(inpe_estado,0) <> 5;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Tabla de Inspección")
	Return True

ELSEIF ll_count > 0 THEN
	MessageBox("Atención", "Número de Inspección se encuentra Cerrada.~rIngrese otro.", + &
	Exclamation!, OK!)
	Return True
END IF


Return False
end function

on w_proc_deta_rehabilita_anulacion.create
int iCurrent
call super::create
this.cb_1=create cb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
end on

on w_proc_deta_rehabilita_anulacion.destroy
call super::destroy
destroy(this.cb_1)
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))


end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]=	Long(ias_campo[1])
//ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetColumn("inpe_numero")
end event

event open;x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)

dw_1.TabOrder = 20


end event

event resize;call super::resize;cb_1.x	= pb_salir.x
cb_1.y	= pb_salir.y + 255
end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_proc_deta_rehabilita_anulacion
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_proc_deta_rehabilita_anulacion
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_proc_deta_rehabilita_anulacion
end type

type pb_primero from w_mant_detalle`pb_primero within w_proc_deta_rehabilita_anulacion
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_proc_deta_rehabilita_anulacion
integer x = 2601
integer y = 348
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_proc_deta_rehabilita_anulacion
integer x = 2601
integer y = 136
end type

type pb_salir from w_mant_detalle`pb_salir within w_proc_deta_rehabilita_anulacion
integer x = 2601
integer y = 568
end type

type dw_1 from w_mant_detalle`dw_1 within w_proc_deta_rehabilita_anulacion
integer x = 73
integer width = 2400
integer height = 784
string dataobject = "dw_gene_archivo_anulacion"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_null

SetNull(ls_null)

ls_columna = dwo.name

CHOOSE CASE ls_columna
	
CASE "inpe_numero"	
	IF existeinspeccion(Long(Data),Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2])) THEN
		dw_1.SetItem(Row, "inpe_numero", Long(ls_null))
		RETURN 1
	ELSE
		istr_mant.argumento[5] = data
	END IF	
	
CASE "paen_numero"
	IF NoExistePallet(data) OR Duplicado(data) THEN
		dw_1.SetItem(Row, "paen_numero", Long(ls_null))
		RETURN 1
	END IF

	pb_acepta.SetFocus()

END CHOOSE
end event

event dw_1::clicked;call super::clicked;CHOOSE CASE dwo.name
	CASE "buscapallet"
		buscapallet()
END CHOOSE
end event

type cb_1 from commandbutton within w_proc_deta_rehabilita_anulacion
boolean visible = false
integer x = 2601
integer y = 796
integer width = 302
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Detalle"
end type

event clicked;IF dw_1.RowCount() > 0 THEN
	istr_mant.Agrega			=	False
	istr_mant.Borra			=	False
	istr_mant.Argumento[6]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"paen_numero"))
   istr_mant.Argumento[7]	=	'1'
	istr_mant.Argumento[8]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"clie_codigo"))
	istr_mant.Argumento[9]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"espe_codigo"))
	istr_mant.Argumento[10]	=	String(dw_1.GetitemNumber(dw_1.GetRow(),"vari_codigo"))
	istr_mant.Argumento[11]	=	'100'
	istr_mant.Argumento[12]	=	'100'
	
	IF IsNull(istr_mant.Argumento[6]) = False AND istr_mant.Argumento[6] <> "" THEN
		OpenWithParm(w_maed_palletencab_consulta, istr_mant)
	END IF
END IF
end event

